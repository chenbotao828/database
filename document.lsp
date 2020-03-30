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
  (lambda (self) 
    (data_put (do self 'DB_name) (do self 'id) self)
    self
  )
)
(defmethod 
  'document
  'delete
  (lambda (self) 
    (data_delete (do self 'DB_name) (do self 'id))
    save
  )
)

(defmethod 
  'document
  'reload
  (lambda (self) 
    (data_get (do self 'DB_name) (do self 'id))
  )
)

(defun do_reload (sym) 
  (check "do_reload" (list sym sym? (eval sym) obj? (do (eval sym) 'id) str?))
  (do_set sym 'reload)
)
(defmethod 
  'document
  'update_set
  (lambda (self key value / db id) 
    (setq db (do self 'DB_name)
          id (do self 'id)
    )
    (data_put db id (al_upsert (data_get db id) key value))
    self
  )
)
(defmethod 
  'document
  'update_unset
  (lambda (self key / db id) 
    (setq db (do self 'DB_name)
          id (do self 'id)
    )
    (data_put db id (al_remove (data_get db id) key))
    self
  )
)
(defmethod 
  'document
  'update_inc
  (lambda (self key amount / db id value) 
    (setq db    (do self 'DB_name)
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
    (setq db    (do self 'DB_name)
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
    (setq db    (do self 'DB_name)
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
    (setq db    (do self 'DB_name)
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
    (setq db    (do self 'DB_name)
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
    (setq db    (do self 'DB_name)
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
    (setq db    (do self 'DB_name)
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
    (setq db    (do self 'DB_name)
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
  'DB_name
  (lambda (self) 
    (strcat "DB_" (str (if (do self 'class) (do self 'class) (do self 'parent))))
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
