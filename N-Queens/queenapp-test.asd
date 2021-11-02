(defsystem "queenapp-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Brett Isken"
  :license ""
  :depends-on ("queenapp"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "queenapp"))))
  :description "Test system for queenapp"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
