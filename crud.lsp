;; database 
;; data: using associated list variables as dict
;; ldata: hash table save with CAD file,  limited amount (<14000) 
(defun data_put (dict key value)
  (check "data_put" (list dict str?))
  (set (read dict) (al_upsert (eval (read dict)) key value))
  )

(defun data_list (dict)
  (check "data_list" (list dict str?))
  (eval (read dict))
  )

(defun data_get (dict key)
  (check "data_get" (list dict str?))
  (al_get (eval (read dict)) key)
  )

(defun data_delete (dict key)
  (check "data_delete" (list dict str?))
  (set (read dict) (al_remove (eval (read dict)) key))
  )

(defun ldata_put (dict key value)
  (check "ldata_put" (list dict (list ename? str?) key (list sym? str?)))
  (vlax-ldata-put dict key value)
  )

(defun ldata_list (dict)
  (check "ldata_list" (list dict (list ename? str?)))
  (vlax-ldata-list dict)
  )

(defun ldata_get (dict key)
  (check "ldata_get" (list dict (list ename? str?) key (list sym? str?)))
  (vlax-ldata-get dict key)
  )

(defun ldata_delete (dict key)
  (check "ldata_get" (list dict (list ename? str?) key (list sym? str?)))
  (vlax-ldata-delete dict key)
  )

;; convertion between data and ldata
(defun data_backup(dict)
  (check "data_store" (list dict str?)) 
  (foreach i (data_list dict) (ldata_put dict (car i) (cdr i)))
  t
  )

(defun data_restore(dict)
  (check "data_restore" (list dict str?)) 
  (foreach i (ldata_list dict) (data_put dict (car i) (cdr i)))
  t
  )
