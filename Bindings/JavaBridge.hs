{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Bindings.JavaBridge

where 

import Preface.Imports
import Preface.FFITemplates

import Bindings.CoreFoundation
import Preface.Stringy

{-

#include "kni.h"

int debug = 0;

static JavaVMOption javaOptions[25];
static void addJavaOption(const char *s) {
  JavaVMOption *jvo = javaOptions;
  while(jvo->optionString) jvo++;
  jvo->optionString = (char *)s;
  jvo->extraInfo = NULL;
  jvo++;
  jvo->optionString = NULL;
  jvo->extraInfo = NULL;
}

K getKerr(jthrowable jt, const char *msg) {
  char buff[4096];
  const char *str;
  jstring s;
  (*env)->ExceptionDescribe(env);
  while(1) {
    jthrowable jtx = (*env)->CallObjectMethod(env, jt, getCause);
    if (jtx == NULL) break;
    jt = jtx;
  }

  s = (*env)->CallObjectMethod(env, jt, toString);
  fprintf(stderr, "got throwable as string\n");
  str = (*env)->GetStringUTFChars(env, s, 0);
  fprintf(stderr, "got throwable as char *\n");
  sprintf(buff, "%s %s", msg, str);
  (*env)->ReleaseStringUTFChars(env,s,str);
  fprintf(stderr, "%s\n",buff);
  (*env)->ExceptionClear(env);
  return kerr(buff);
}

static K doStartVM(char *cp, int p);

DLLEXPORT K VMRunningQ() {
  return gi( env == NULL ? 0 : 1 );
}

-}

{-
jobject JNICALL jksk(JNIEnv *env, jobject self, jstring js, jobjectArray joa) {
  jobject jres;
  const char *str = (*env)->GetStringUTFChars(env, js, 0);
  K arg = kFromJ(joa);
  K res = ksk( (char *)str, arg);
  (*env)->ReleaseStringUTFChars(env,js,str);

  if(6 == res->t && NULL != *(char **)(KC(res)))printf("*** K error: %s\n",*(char **)(KC(res)));

  box(res, &jres);
  cd(res);
  return jres;
}
-}

-- foreign import ccall "curl/easy.h curl_easy_getinfo" c_curl_easy_getinfo :: CurlStruct -> CLong -> Ptr () -> IO CInt

foreign import ccall "JavaVM/jni.h JNI_CreateJavaVM" jni_CreateJavaVM :: Ptr JavaVM -> Ptr JNIEnv -> Ptr JavaVMInitArgs -> IO CInt

{-
It turns out that JNI_CreateJavaVM() pops up the dialog only if the VM library/function has been loaded using dlopen/dlsym.  The problem does not happen if the function is loaded using the CFBundle APIs (CFBundleCreate/CFBundleGetFunctionPointerForName). Somehow loading the JDK 1.7 bundle avoids the problem.
 - 
 - Ergo: if I use CFBundleCreate / CFBundleGetFunctionPointerForName
 - 
 - to acquire a function pointer to JNI_CreateJavaVM, then I can use that function pointer
 - to call JNI_CreateJavaVM
 -
 -
 - However the fix of mucking with the Info.plist to add the JNI capability means that
 - I don't have to deal with that right away
 -
 -}
type CjvmT = Ptr JavaVM -> Ptr JNIEnv -> Ptr JavaVMInitArgs -> IO CInt
foreign import ccall "dynamic" mkCJVM :: FunPtr CjvmT -> CjvmT

{-
typedef struct JavaVMOption {
    char *optionString;
    void *extraInfo;
} JavaVMOption;

typedef struct JavaVMInitArgs {
    jint version;

    jint nOptions;
    JavaVMOption *options;
    jboolean ignoreUnrecognized;
} JavaVMInitArgs;

-}
type VoidPtr = Ptr ()
type JavaVMOptionArray = Ptr [JavaVMOption]

[storable|JavaVMOption optionString CString extraInfo VoidPtr|]
[storable|JavaVMInitArgs version CLong nOptions CLong options JavaVMOptionArray ignoreUnrecognized CChar|]

data JavaVM_struct
type JavaVM = Ptr JavaVM_struct

type JNIEnv_struct = Ptr ()
type JNIEnv = Ptr JNIEnv_struct

jni_VERSION_1_6 = 0x00010006


{- | Passing in a String representing the classpath, and a port to be used for attaching the
 -   Java debugger, this will initialize the Java VM with that classpath and ready to be 
 -   debugged.
 -}
startVM :: String -> Int -> IO (Either Int (JavaVM, JNIEnv))
startVM classpath debugPort = do

  
{-
  char buf[4096];
  char cpb[8192];
  int r;
  JavaVMInitArgs vm_args;
  JavaVMOption* opt;

  javaOptions[0].optionString = NULL;
  addJavaOption("-Xmx50m");
  addJavaOption("-XX:+UseCompressedOops");
  addJavaOption("-Djava.awt.headless=true");
  addJavaOption("-Dfile.encoding=UTF-8");
  //  addJavaOption("-verbose:jni");

// if debugging ...
  if (debugPort != 0) {
    // addJavaOption("-Xint");
    // addJavaOption("-Xdebug");
    char cc = 'n';
    if (debugPort < 0) { cc = 'y'; debugPort = -debugPort; }
    sprintf(buf, "-agentlib:jdwp=transport=dt_socket,server=y,suspend=%c,address=%d", cc, debugPort);
    //    sprintf(buf, "-Xrunjdwp:transport=dt_socket,onuncaught=y,address=%s:%d","localhost",debugPort);
    //    sprintf(buf, "-Xrunjdwp:transport=dt_socket,suspend=y,address=%s:%d","localhost",debugPort);
    addJavaOption(buf);
  }

  strcpy(cpb,"-Djava.class.path=");
  strcat(cpb, cp);
  addJavaOption(cpb);

  vm_args.version = JNI_VERSION_1_6;
  vm_args.options = javaOptions;
  vm_args.nOptions = 0;
  for( opt = vm_args.options; opt->optionString; opt++, vm_args.nOptions++) {
    if (debug) printf("java vm option: %s\n", opt->optionString);
    };
  vm_args.ignoreUnrecognized = JNI_TRUE;
-}
  alloca $ \vm_args -> do
    poke vm_args (JavaVMInitArgs jni_VERSION_1_6 0 nullPtr 1) 
    alloca $ \vm -> 
      alloca $ \env ->
        do r <- jni_CreateJavaVM vm env vm_args
           v <- peek vm
           e <- peek env
           return $ if r /= 0 then Left (fromIntegral r) else Right (v, e)

  -- kniInit();

{-
  /****************************************************/
  /* now register the K callback method               */
  {
    JNINativeMethod nm;
    nm.name="doRunK";
    nm.signature="(Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/Object;";
    nm.fnPtr=jksk;
    (*env)->RegisterNatives(env,kniClass,&nm,1);
    CHECK_EXCEPTION("registering Kni.doRunK");
  }
-}

foreign import ccall "dynamic" mkFunX :: FunPtr (JNIEnv -> IO CLong) -> (JNIEnv -> IO CLong)

type MkFun6 = (JNIEnv -> CString -> IO JClass_)
foreign import ccall "dynamic" mkFunY :: FunPtr MkFun6 -> MkFun6

type MkFun33 = (JNIEnv -> JClass_ -> CString -> CString -> IO JMethodID_)
foreign import ccall "dynamic" mkFun33 :: FunPtr MkFun33 -> MkFun33

jniGetVersion :: JNIEnv -> IO CLong
jniGetVersion e = do
  y <- peek e
  z <- peek (advancePtr (castPtr y) 4)
  let a = castPtrToFunPtr z 
      b = mkFunX (castFunPtr a)
  b e
  
type JClass_ = Ptr ()
type JClass = JClass_

jniFindClass :: JNIEnv -> String -> IO JClass
jniFindClass e n = do
  y <- peek e
  z <- peek (advancePtr (castPtr y) 6)
  let a = castPtrToFunPtr z 
      b = mkFunY (castFunPtr a)
  withCString n (b e)

type JMethodID_ = Ptr ()
type JMethodID = JMethodID_

jniGetMethodID :: JNIEnv -> JClass -> String -> String -> IO JMethodID
jniGetMethodID e c n t = do
  y <- peek e
  z <- peek (advancePtr (castPtr y) 33)
  let a = castPtrToFunPtr z 
      b = mkFun33 (castFunPtr a)
  withCString t (\tt -> withCString n (\nn -> (b e c) nn tt))
 
type JObject_ = Ptr ()
type JObject = JObject_

type JString_ = JObject_
type JString = JString_

data JValue = JObject JObject_ | JClass JClass_ | JInt Int32 | JDouble Double | JFloat Float | JShort Int16 | JBoolean Int8
  deriving (Show, Eq)

{-
typedef struct _jobject *jobject;
typedef jobject jclass;
typedef jobject jthrowable;
typedef jobject jstring;
typedef jobject jarray;
typedef jarray jbooleanArray;
typedef jarray jbyteArray;
typedef jarray jcharArray;
typedef jarray jshortArray;
typedef jarray jintArray;
typedef jarray jlongArray;
typedef jarray jfloatArray;
typedef jarray jdoubleArray;
typedef jarray jobjectArray;
-}

type MkFun36 = JNIEnv -> JObject -> JMethodID -> Ptr () -> IO JObject
foreign import ccall "dynamic" mkFun36 :: FunPtr MkFun36 -> MkFun36

instance Storable JValue where
  sizeOf _ = 8
  alignment _ = 8
  poke e ev = case ev of
     JObject x -> poke (castPtr e) x
     JClass x -> poke (castPtr e) x
     JInt x -> poke (castPtr e) x
     JDouble x -> poke (castPtr e) x
     JFloat x -> poke (castPtr e) x
     JShort x -> poke (castPtr e) x

  peek e = let n = f e n in n
             where f e n = do a <- n
                              case a of
                                JObject n -> JObject <$> peek (castPtr e)
                                JClass n -> JClass <$> peek (castPtr e)
                                JInt n -> JInt <$> peek (castPtr e)
                                JDouble n -> JDouble <$> peek (castPtr e)
                                JFloat n -> JFloat <$> peek (castPtr e)
                                JShort n -> JShort <$> peek (castPtr e)

jniCallObjectMethod :: JNIEnv -> JObject -> JMethodID -> [JValue] -> IO JObject
jniCallObjectMethod e o m y = do
  yy <- peek e
  z <- peek (advancePtr (castPtr yy) 36)
  let a = castPtrToFunPtr z 
      b = mkFun36 (castFunPtr a)
  withArray y $ \j -> (b e o m (castPtr j))

{-

jclass cls = (*env)->FindClass("java/lang/Class"); 
MethodID mid_getName = (*env)->GetMethodID(env, cls, "getName", "()Ljava/lang/String;");
jstring name = (*env)->CallObjectMethod(env, cls, mid_getName);

-}

type MkFun169 = JNIEnv -> JString -> Ptr CChar -> IO CString
foreign import ccall "dynamic" mkFun169 :: FunPtr MkFun169 -> MkFun169

type MkFun170 = JNIEnv -> JString -> CString -> IO ()
foreign import ccall "dynamic" mkFun170 :: FunPtr MkFun170 -> MkFun170

jniReleaseStringUTFChars :: JNIEnv -> JString -> CString -> IO ()
jniReleaseStringUTFChars e j s = do
   error "jniReleaseStringUTFChars not yet implemented"

jniGetStringUTFChars :: JNIEnv -> JString -> IO String
jniGetStringUTFChars e s = do
  yy <- peek e
  z <- peek (advancePtr (castPtr yy) 169)
  let a = castPtrToFunPtr z
      b = mkFun169 (castFunPtr a)
  (resy, jby) <- alloca $ \x -> do 
                           res <- b e s x
                           jb <- peek x
                           return (res, jb)
  resb <- packCString resy
  -- if jby then jniReleaseStringUTFChars
  return (asString resb)

{-
    const char* (JNICALL *GetStringUTFChars)
      (JNIEnv *env, jstring str, jboolean *isCopy);
    void (JNICALL *ReleaseStringUTFChars)
      (JNIEnv *env, jstring str, const char* chars);


-}


 
{-
static K kObjFromJ(jobject obj);
static K kObjFromJ(jobject obj) {
    jobject go = (*env)->NewGlobalRef(env, obj);
  K rko = gtn(-3, 12+sizeof(jobject));
  strncpy(KC(rko), "Java Object\0",12);
  strncpy(KC(rko)+12, (void *)&go, sizeof(jobject));
return rko;
}

jclass getKcls(K arg) {
  jclass kcls;
  switch( arg->t) {
  case 0: kcls=objectArrayClass; break;
  case 1: kcls=integerClass; break;
  case 2: kcls=doubleClass; break;
  case 3: kcls=byteClass; break;
  case 4: kcls=stringClass; break;
  case 5: kcls=hashClass; break; // maybe mapClass?
  case -1: kcls=integerArrayClass; break;
  case -2: kcls=doubleArrayClass; break;
  case -3: kcls=byteArrayClass; break;
  case -4: kcls=stringArrayClass; break;
  case 6: kcls=objectClass; break;
  default:
    printf("Unknown K class as method argument\n");
    return NULL;
  }
  return kcls;
}
-}

-- foreign import ccall "JavaVM/jni.h FindClass

{-
jobject getMethodX( const char *clsn, const char *mthn, K arg) {
  jclass jc;
  jobject meth;
  jmethodID mthid;
  char buf[4096];
  char *dot;
  jstring jmthn;
  jobjectArray kclsa;
  int i;

  sprintf(buf, "L%s;", clsn);
  dot = buf;
  while(1) {
    dot = strchr(dot, '.');
    if (dot == NULL) break;
    *dot = '/';
  }
  jc = (*env)->FindClass(env, buf);
  CHECK_EXCEPTION_N("finding class for method");

  jmthn = (*env)->NewStringUTF(env, mthn);
  CHECK_EXCEPTION_N("finding method for invocation");

  if (arg->t == 0) {
    kclsa = (*env)->NewObjectArray(env, arg->n, objectClass, NULL);
    CHECK_EXCEPTION_N("creating multiple argument type array");
    for(i=0;i<arg->n; i++) {
      (*env)->SetObjectArrayElement(env, kclsa, (jint) i, getKcls( KK(arg)[i] ));
      CHECK_EXCEPTION_N("setting class of  argument in method argument array");
    }
  } else {
    jclass kcls = getKcls(arg);
    kclsa = (*env)->NewObjectArray(env, 1, objectClass, kcls);
    CHECK_EXCEPTION_N("creating single argument type array");
  }

  meth = (*env)->CallObjectMethod(env, jc, getMethod, jmthn, kclsa);
  CHECK_EXCEPTION_N("using reflection to find method");

//  (*env)->DeleteLocalRef(env, jc);
//  (*env)->DeleteLocalRef(env, jmthn);
//  (*env)->DeleteLocalRef(env, kclsa);

  //  mthid = (*env)->FromReflectedMethod(env, meth);
  //  if ((*env)->ExceptionOccurred(env)) return NULL;
  return meth;
}

/* Use reflection so that I don't need to specify the returned type */
DLLEXPORT K callStatic(K clsk, K mthk, K arg) {
  char *clsnam, *mthnam;
    char mtyp[4096];
    jclass classx;
    char *dot;
    jmethodID mthid;
    char *typstr;
    jvalue argl[1];
    K r,rko;
    jsize an;
    jint *api;
    jboolean *apz;
    int i;
    jshort *aps;
    jbyte *apb;
    jdouble *apd;
    jobject reso;
    jobject meth;
    jobject jarg = NULL;
	jint okq;
	jobject result;

    // char *restyp;
    
    
    if (debug) printf("callStatic invoked\n");
    if (4 != clsk->t) R kerr("first arg must be a symbol");
    if (4 != mthk->t) R kerr("second arg must be a symbol");
    clsnam = Ks(clsk);
    mthnam = Ks(mthk);
    if (debug) printf("clsnam=%s, mthnam=%s\n", clsnam, mthnam);

		okq = (*env)->PushLocalFrame(env, 1000000);
		if (okq) {
			printf("pushLocalFrame failed\n");
			return kerr("pushLocalFrame failed");
		}
	
    r = box(arg, &jarg);
    if (r) return r;

    meth = getMethodX(clsnam, mthnam, arg);
    if (meth == NULL) return kerr("method not found");
    if (debug) printf("method found\n");


    if (0 != arg->t) {
      jarg = (*env)->NewObjectArray(env, 1, objectClass, jarg);
      CHECK_EXCEPTION("creating argument type array for invocation");
    }
    if (debug) printf("method argument array created\n");

    result = (*env)->CallObjectMethod(env, meth, invoke, NULL, jarg);
    CHECK_EXCEPTION("Executing method");
    if (debug) printf("method executed\n");

//	(*env)->DeleteLocalRef(env, meth);
//	if (jarg) (*env)->DeleteLocalRef(env, jarg);
    rko = kFromJ(result); 

//	(*env)->DeleteLocalRef(env, result);

    (*env)->PopLocalFrame(env, NULL);
	
    return rko;
}

// ====================================================================================
K deleteObject(K objk) {

    jobject obj;

  if ( objk->n != 12 + sizeof(jobject) || 0 != strncmp(KC(objk), "Java Object\0", 12))
    R kerr("arg must represent a Java Object");
  strncpy((void*)&obj, KC(objk)+12, sizeof(jobject));	
  (*env)->DeleteGlobalRef(env, obj);
  return gn();
}

K newObject(K clsk, K arg) {
  char*clsnam = Ks(clsk);
  char mtyp[4096];
  char *typstr;
  jclass classx;
  jmethodID mthid;
  jvalue argl[1];
  K r;
  jobject obj;
  K res;

  if (4 != clsk->t) R kerr("first arg must be a symbol");
  

  classx = (*env)->FindClass(env, clsnam);
  CHECK_EXCEPTION("Error loading class");

  r = jFromK(&argl[0], &typstr, arg);
  if (r) return r;
  sprintf(mtyp,"(%s)V",typstr);
  mthid = (*env)->GetMethodID(env, classx, "<init>", mtyp);
  CHECK_EXCEPTION("Error finding constructor");

  obj = (*env)->NewObjectA(env, classx, mthid, argl);
  CHECK_EXCEPTION("Executing constructor");
  res = kObjFromJ(obj);
//  (*env)->DeleteLocalRef(env, obj);
//  (*env)->DeleteLocalRef(env, (jobject)classx);
//  (*env)->DeleteLocalRef(env, (jobject)mthid);
  return res;
}


-}


