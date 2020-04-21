(deftest "document")

(defcls 'test_document 'document '(a b c))

(setq td (test_document 1 2 (list 3)))
(assert-eq '1 '(do td 'a))
(do td 'save)
(do_reload 'td)
(setq td_id (do td 'id))
(assert-eq ''str '(type td_id))

(do td '(update_set a 10))
(do_reload 'td)
(assert-eq '10 '(do td 'a))

(do td '(update_unset a))
(do_reload 'td)
(assert-eq 'nil '(do td 'a))

(do td '(update_inc b 2))
(do_reload 'td)
(assert-eq '4 '(do td 'b))

(do td '(update_dec b 2))
(do_reload 'td)
(assert-eq '2 '(do td 'b))

(do td '(update_push b 2))
(do_reload 'td)
(assert-eq '(list 2 2) '(do td 'b))
(do td '(update_push c 2))
(do_reload 'td)
(assert-eq '(list 2 3) '(do td 'c))
(do td '(update_set b 2))
(do_reload 'td)

(do td '(update_push_all c (0 1)))
(do_reload 'td)
(assert-eq '(list 0 1 2 3) '(do td 'c))

(do td '(update_pop c))
(do_reload 'td)
(assert-eq '(list 1 2 3) '(do td 'c))


(do td '(update_pull c 1))
(do_reload 'td)
(assert-eq '(list 2 3) '(do td 'c))

(do td '(update_pull_all c (2 3)))
(do_reload 'td)
(assert-eq 'nil '(do td 'c))

(do td '(update_push_all c (1 2 3)))
(do_reload 'td)
(assert-eq '(list 1 2 3) '(do td 'c))
(do td '(update_add_to_set c 1))
(do_reload 'td)
(do td '(update_add_to_set c 0))
(do_reload 'td)
(assert-eq '(list 0 1 2 3) '(do td 'c))

(assert-eq '"DB_TEST_DOCUMENT" '(do td 'DB_name))
(assert-eq '1 '(len (do td 'objects)))
;; (assert-eq '"DB_TEST_DOCUMENT" '(do td 'DB_name))

(do_reload 'td)
(do td (list 'update_all (al_make '(c 1 b 1))))
(do_reload 'td)
(assert-eq '1 '(do td 'c))
(assert-eq '1 '(do td 'b))

(setq td nil
      td_id nil
      _test_document nil
      test_document nil
      DB_test_document nil
      )
