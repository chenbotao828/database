(defcls 'document nil nil)
(defmethod 
  'document
  'init
  (lambda (self / id db) 
    (setq id   (objectid)
          self (cons (cons 'id id) self)
    )
    (list (cons 'id id))
  )
)

(defmethod 
  'document
  'save
  (lambda (self / id db_name self_db ret) 
    (setq id      (do self 'id)
          db_name (do self 'DB_name)
          self_db (data_get DB_name id)
    )
    (if self_db 
      (data_put db_name id (setq ret (al_upserts self_db self)))
      (data_put db_name id (setq ret self))
    )
    ret
  )
)

(defmethod 
  'document
  'delete
  (lambda (self) 
    (data_delete (do self 'DB_name) (do self 'id))
    nil
  )
)

(defmethod 
  'document
  'reload
  (lambda (self / ret) 
    (setq ret (data_get (do self 'DB_name) (do self 'id)))
    (if ret ret self)
  )
)

(defun do_reload (sym) 
  (check "do_reload" (list sym sym? (eval sym) obj? (do (eval sym) 'id) str?))
  (do_set sym 'reload)
)

(defmethod 
  'document
  'update
  (lambda (self paras / sign _paras) 
    (setq sign   (read (strcat "update_" (str (car paras))))
          _paras (cdr paras)
    )
    (do self (cons sign _paras))
  )
)

(defmethod 
  'document
  'update_set
  (lambda (self key value / db id) 
    (setq db   (do self 'DB_name)
          id   (do self 'id)
          self (do self 'reload)
    )
    (data_put db id (al_upsert self key value))
  )
)
(defmethod 
  'document
  'update_unset
  (lambda (self key / db id) 
    (setq db   (do self 'DB_name)
          id   (do self 'id)
          self (do self 'reload)
    )
    (data_put db id (al_remove (data_get db id) key))
    self
  )
)
(defmethod 
  'document
  'update_inc
  (lambda (self key amount / db id value) 
    (setq self  (do self 'reload)
          db    (do self 'DB_name)
          id    (do self 'id)
          value (do self key)
    )
    (check "document update_inc" (list amount num? value num?))
    (data_put db id (al_upsert (data_get db id) key (+ value amount)))
    self
  )
)
(defmethod 
  'document
  'update_dec
  (lambda (self key amount / db id value) 
    (setq self  (do self 'reload)
          db    (do self 'DB_name)
          id    (do self 'id)
          value (do self key)
    )
    (check "document update_dec" (list amount num? value num?))
    (data_put db id (al_upsert (data_get db id) key (- value amount)))
    self
  )
)
(defmethod 
  'document
  'update_push
  (lambda (self key item / db id value) 
    (setq self  (do self 'reload)
          db    (do self 'DB_name)
          id    (do self 'id)
          value (do self key)
          value (if (or (nil? value) (list? value)) 
                  (append (list item) value)
                  (append (list item) (list value))
                )
    )
    (data_put db id (al_upsert (data_get db id) key value))
    self
  )
)
(defmethod 
  'document
  'update_push_all
  (lambda (self key items / db id value) 
    (setq self  (do self 'reload)
          db    (do self 'DB_name)
          id    (do self 'id)
          value (do self key)
          value (if (or (nil? value) (list? value)) 
                  (append items value)
                  (append items (list value))
                )
    )
    (data_put db id (al_upsert (data_get db id) key value))
    self
  )
)
(defmethod 
  'document
  'update_pop
  (lambda (self key / db id value) 
    (setq self  (do self 'reload)
          db    (do self 'DB_name)
          id    (do self 'id)
          value (do self key)
    )
    (if (or (nil? value) (list? value)) 
      (data_put db id (al_upsert (data_get db id) key (cdr value)))
    )
    self
  )
)
(defmethod 
  'document
  'update_pull
  (lambda (self key item / db id value) 
    (setq self  (do self 'reload)
          db    (do self 'DB_name)
          id    (do self 'id)
          value (do self key)
    )
    (if (and (assoc key self) (or (nil? value) (list? value))) 
      (data_put 
        db
        id
        (al_upsert 
          (data_get db id)
          key
          (vl-remove item value)
        )
      )
    )
    self
  )
)
(defmethod 
  'document
  'update_pull_all
  (lambda (self key items / db id value) 
    (setq self  (do self 'reload)
          db    (do self 'DB_name)
          id    (do self 'id)
          value (do self key)
    )
    (if (and (assoc key self) (or (nil? value) (list? value))) 
      (data_put 
        db
        id
        (al_upsert 
          (data_get db id)
          key
          (foreach item items 
            (setq value (vl-remove item value))
          )
        )
      )
    )
    self
  )
)
(defmethod 
  'document
  'update_add_to_set
  (lambda (self key item / db id value) 
    (setq self  (do self 'reload)
          db    (do self 'DB_name)
          id    (do self 'id)
          value (do self key)
    )
    (if 
      (and (or (list? value) (nil? value)) 
           (not (member item value))
      )
      (data_put 
        db
        id
        (al_upsert 
          (data_get db id)
          key
          (append (list item) value)
        )
      )
    )
    self
  )
)

(defmethod 
  'document
  'update_all
  (lambda (self al) 
    (setq self (do self 'reload))
    (data_put (do self 'DB_name) (do self 'id) (al_upserts self al))
  )
)

(defmethod 
  'document
  'DB_name
  (lambda (self) 
    (strcat "DB_" 
            (str (if (do self 'class) (do self 'class) (do self 'parent)))
    )
  )
)
(defmethod 
  'document
  'objects
  (lambda (self) 
    (al_values (data_list (do self 'DB_name)))
  )
)
(defmethod 
  'document
  'where
  (lambda (self where_func) 
    (where (do self 'objects) where_func)
  )
)
