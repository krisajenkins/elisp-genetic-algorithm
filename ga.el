(load-file "levenshtein.el")

(defun ga/remove-duplicates
  (coll)
  (delq nil (delete-dups coll)))

(defun ga/safe-substring
  (string from)
  (if (< (length string) from)
	  ""
	(substring string from)))

(defvar ga/chars
  (concat
   "abcdefghijklmnopqrstuvwxyz"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   " ,.!?"))

(defun ga/random-char
  ()
  (let ((n (random (length ga/chars))))
	(substring ga/chars n (+ 1 n))))

(defun ga/random-string
  (length)
  (if (<= length 0)
	  ""
	(concat (ga/random-char)
			(ga/random-string (- length 1)))))

(defun ga/create-pool
  (pool-size initial-length)
  "Create the initial list."
  (if (<= pool-size 0)
	  (list)
	(cons (ga/random-string initial-length)
		  (ga/create-pool (- pool-size 1) initial-length))))

(defun ga/fitness
  (target gene)
  (levenshtein-distance target gene))

(defun ga/rank
  (target pool)
  (sort pool (lambda (gene1 gene2)
			   (if (< (ga/fitness target gene1)
					  (ga/fitness target gene2))
				   t))))

(defun ga/breed
  (gene1 gene2)
  "Breeds two genes. Returns one child."
  (let ((slice-size (random 3))
		(head-gene (if (zerop (random 2)) gene1 gene2)))
	(if (< (length head-gene) slice-size)
		gene1
	  (concat (substring head-gene 0 slice-size)
			  (ga/breed (ga/safe-substring gene1 slice-size)
						(ga/safe-substring gene2 slice-size))))))

(defun ga/mutate
  (one-in-x gene)
  "There is a one-in-x chance of any letter in a gene being mutated. Mutations are: change/delete/insert"
  (if (zerop (length gene))
	  gene
	(concat (case (random (* one-in-x 4))
			  (0 (ga/random-char))		; Change
			  (1 (concat (ga/random-char) (substring gene 0 1))) ; Insert
			  (2 "")							; Delete
			  (otherwise (substring gene 0 1)))	; No mutation
			(ga/mutate one-in-x (substring gene 1)))))

;;; TODO This function should de-duplicate.
;;; TODO This function return as many genes as are in the supplied pool.
(defun ga/evolve
  (target pool one-in-x)
  "Given a gene pool, evolves the next generation."
  (let* ((parents (ga/rank target pool))
		 (children (list (ga/breed (nth 0 parents) (nth 1 parents))
						 (ga/breed (nth 0 parents) (nth 2 parents))
						 (ga/breed (nth 0 parents) (nth 3 parents))
						 (ga/breed (nth 0 parents) (nth 4 parents))

						 (ga/breed (nth 1 parents) (nth 2 parents))
						 (ga/breed (nth 1 parents) (nth 3 parents))
						 (ga/breed (nth 1 parents) (nth 4 parents))

						 (ga/breed (nth 2 parents) (nth 3 parents))
						 (ga/breed (nth 2 parents) (nth 4 parents))
						 (ga/breed (nth 2 parents) (nth 5 parents))
						 )))
	(message "Fittest 2: %S" (car parents))
	(cons (nth 0 parents)
		  (mapcar (apply-partially 'ga/mutate one-in-x)
				  children))))

(defun ga/main
  (target threshold)
  "CREATE A RANDOM GENEPOOL, AND EVOLVE WITH A FITNESS FUNCTION THAT LIKES THE TARGET string.
   Stop as soon as the fittest gene is within the threshold of the target."
  (let ((pool (ga/create-pool 10 10))
		(generation 0))
	(while (< threshold (levenshtein-distance target (car pool)))
	  (setq pool (ga/evolve target pool 5))
	  (setq generation (+ 1 generation)))
	(format "Done in %d generation(s). Target '%s'. Best '%s'. Threshold: %d" generation target (car pool) threshold)))

(ga/main "super" 0)
(ga/main "tasty pies" 0)
