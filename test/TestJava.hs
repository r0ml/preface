{-# LANGUAGE NoImplicitPrelude #-}

import Preface

main = do
  Right (v, e) <- startVM "/" 0

  c <- jniFindClass e "java/lang/Class"
  m <- jniGetMethodID e c "getName" "()Ljava/lang/String;"
  n <- jniCallObjectMethod e c m []
  nx <- jniGetStringUTFChars e n
  print (c,m, n, nx)

{-
jclass cls = (*env)->FindClass("java/lang/Class"); 
MethodID mid_getName = (*env)->GetMethodID(env, cls, "getName", "()Ljava/lang/String;");
jstring name = (*env)->CallObjectMethod(env, cls, mid_getName);

-}

