require 'formula'

class Picosat < Formula
  homepage 'http://fmv.jku.at/picosat/'
  url 'http://fmv.jku.at/picosat/picosat-953.tar.gz'
  sha1 '6040155c929e50cdaaf95c76e37624c41cc9bf0f'

  def patches
    # Fix the dynamic shared library.
    DATA
  end

  def install
    system "./configure -shared"
    system "make"
    bin.install "picosat", "picogcnf", "picomcs", "picomus"
    lib.install "libpicosat.a", "libpicosat.dylib"
    include.install "picosat.h"
  end
end


__END__
diff --git a/configure b/configure
index ca5ec77..fe9e162 100755
--- a/configure
+++ b/configure
@@ -108,7 +108,7 @@ fi
 TARGETS="picosat picomcs picomus picogcnf libpicosat.a"
 if [ $shared = yes ]
 then
-  TARGETS="$TARGETS libpicosat.so"
+  TARGETS="$TARGETS libpicosat.dylib"
   CFLAGS="$CFLAGS -fPIC"
 fi
 echo "targets ... $TARGETS"
diff --git a/makefile.in b/makefile.in
index 6621bb8..211031a 100644
--- a/makefile.in
+++ b/makefile.in
@@ -49,8 +49,7 @@ libpicosat.a: picosat.o version.o
 	ar rc $@ picosat.o version.o
 	ranlib $@
 
-SONAME=-Xlinker -soname -Xlinker libpicosat.so
-libpicosat.so: picosat.o version.o
-	$(CC) $(CFLAGS) -shared -o $@ picosat.o version.o $(SONAME)
+libpicosat.dylib: picosat.o version.o
+	$(CC) $(CFLAGS) -shared -install_name $@ -o $@ picosat.o version.o
 
 .PHONY: all clean

