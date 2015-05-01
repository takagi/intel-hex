#|
  This file is a part of intel-hex project.
  Copyright (c) 2015 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage intel-hex-test-asd
  (:use :cl :asdf))
(in-package :intel-hex-test-asd)

(defsystem intel-hex-test
  :author "Masayuki Takagi"
  :license "MIT"
  :depends-on (:intel-hex
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "intel-hex"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove.asdf) c)
                    (asdf:clear-system c)))
