A recent change in Emacs 28 has made the third argument (WHEN) of define-obsolete-function-alias mandatory. Code that depends on list-utils.el gets repeated error messages because of two uses of that function with only the old-name and new-name signature. The problem is easy to fix: add a date to the two definitions. The blame annotation suggests 2012-10-31 as a good date (as good as any other) to use for this purpose. 

Removing the obsolete names may be easier, but keeping old code alive unless utterly unavoidable is a good goal.

I will try to upload a file fixed as below.

--César Quiroz (cesar.quiroz@gmail.com)

diff --git a/list-utils.el b/list-utils.el
index 178bc5341..99da1cfac 100644
--- a/list-utils.el
+++ b/list-utils.el
@@ -372,7 +372,8 @@ Modifies LIST and returns the modified value."
      list)
     (t
      list)))
-(define-obsolete-function-alias 'list-utils-make-proper 'list-utils-make-proper-inplace)
+(define-obsolete-function-alias
+  'list-utils-make-proper 'list-utils-make-proper-inplace "2012-10-31")

 ;;;###autoload
 (defun list-utils-make-improper-copy (list &optional tree recur-internal)
@@ -431,7 +432,8 @@ Modifies LIST and returns the modified value."
      list)
     (t
      list)))
-(define-obsolete-function-alias 'list-utils-make-improper 'list-utils-make-improper-inplace)
+(define-obsolete-function-alias
+  'list-utils-make-improper 'list-utils-make-improper-inplace "2012-10-31")

 ;;;###autoload
 (defun list-utils-linear-subseq (list &optional cycle-length)

-- [http://cesar.quiroz@gmail.com Cesar Quiroz] 2021-01-06 09:48 UTC


----

A recent change in Emacs 28 has made the third argument (WHEN) of define-obsolete-function-alias mandatory. Code that depends on list-utils.el gets repeated error messages because of two uses of that function with only the old-name and new-name signature. The problem is easy to fix: add a date to the two definitions. The blame annotation suggests 2012-10-31 as a good date (as good as any other) to use for this purpose. 

Removing the obsolete names may be easier, but keeping old code alive unless utterly unavoidable is a good goal.

I will try to upload a file fixed as below.

--César Quiroz (cesar.quiroz@gmail.com)

{{{
diff --git a/list-utils.el b/list-utils.el
index 178bc5341..99da1cfac 100644
--- a/list-utils.el
+++ b/list-utils.el
@@ -372,7 +372,8 @@ Modifies LIST and returns the modified value."
      list)
     (t
      list)))
-(define-obsolete-function-alias 'list-utils-make-proper 'list-utils-make-proper-inplace)
+(define-obsolete-function-alias
+  'list-utils-make-proper 'list-utils-make-proper-inplace "2012-10-31")

 ;;;###autoload
 (defun list-utils-make-improper-copy (list &optional tree recur-internal)
@@ -431,7 +432,8 @@ Modifies LIST and returns the modified value."
      list)
     (t
      list)))
-(define-obsolete-function-alias 'list-utils-make-improper 'list-utils-make-improper-inplace)
+(define-obsolete-function-alias
+  'list-utils-make-improper 'list-utils-make-improper-inplace "2012-10-31")

 ;;;###autoload
 (defun list-utils-linear-subseq (list &optional cycle-length)
}}}

-- [https://www.linkedin.com/in/cesarquiroz/ Cesar Quiroz] 2021-01-06 09:53 UTC

