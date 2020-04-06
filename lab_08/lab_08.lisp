; ----------------------- ЗАДАНИЕ 1 --------------------------
; Написать функцию, которая по своему списку-аргументу Ist определяет
; является ли он палиндромом (то есть равны ли Ist и reverse Ist).

; создает список обратной последовательности
(defun reverse-list (list) 
    (reduce 
        (lambda (tmp result)                 ; tmp - текущий элемент 
                (append result (list tmp))   ; result - список
        ) list :from-end t :initial-value nil
    )
)

; проверка на палиндром 
(defun is-palindrom (list)
    (equal list (reverse-list list))
)

; Проверка
(reverse-list '(1 2 3))
(reverse-list '(a b c d a))
(reverse-list nil)

(is-palindrom '(1 2 3 2 1))
(is-palindrom '(1 2 3 2 2))

; альтернативная проверка списков
(defun my-and (list)
    (reduce 
        (lambda(prev tmp) 
                (if (null tmp) tmp prev)
        ) list :initial-value t
    )
)

(defun cmp-lists (list1 list2)
    (if (equal (length list1) (length list2))
        (funcall #'my-and (mapcar #'equal list1 list2))
        nil
    )
)

(defun is-palindrom-2 (list)
    (cmp-lists list (reverse-list list))
)
; ----------------------- ЗАДАНИЕ 2 --------------------------
; Напишите функцию swap-first-last, которая переставляет в 
; списке-аргументе первый и последний элементы.

; Первый вариант 
(defun swap-first-last (list)
    (reduce 
        (lambda (result tmp)
            (if (equal (length result) (- (length list) 1)) 
                (append result (list (car list)))
                (append result (list tmp))
            )
        ) (cdr list) :initial-value (last list)
    ) 
)

; Проверка
(swap-first-last '(1 2 3 4 5))


; Второй вариант
(defun swap-first-last
    (reduce 
        (lambda (result tmp)
            (if (equal (length result) (- (length list) 2)) 
                (append (append (list tmp) result) (list (car list)))
                (append result (list tmp))
            )
        ) (cdr list) :initial-value nil
    ) 
)

; ----------------------- ЗАДАНИЕ 3 --------------------------
; Напишите функцию swap-two-ellement, которая переставляет в 
; списке- аргументе два указанных своими порядковыми номерами
; элемента в этом списке.

; первый вариант get-by-number
(defun get-by-number (n list)
    (if (equal n 0)
        (car list)
        (get-by-number (- n 1) (cdr list))
    )
)

; второй вариант get-by-number
(defun my-member (n list)
    (reduce 
        (lambda (result tmp)
            (if (equal (length result) (- (length list) n))
                result 
                (cdr result)
            ) 
        ) list :initial-value list
    )
)

; получить элемент списка по его номеру
(defun get-by-number (n list)
    (car (my-member n list))
)

; основная функция, переставляет два элемента 
; заданные своими номерами left right 
(defun swap-two-ellement (left right list)
    (reduce 
        (lambda (result tmp)
            (cond 
                (   
                    (equal (length result) left)  
                    (append result (list (get-by-number right list)))
                ) 

                (   
                    (equal (length result) right)   
                    (append result (list (get-by-number left list)))
                ) 

                (   
                    t 
                    (append result (list tmp))
                )
            )
        ) list :initial-value nil
    )
)

; ----------------------- ЗАДАНИЕ 4 --------------------------
; Напишите две функции, swap-to-left и swap-to-right, 
; которые производят круговую перестановку в списке-аргументе
; влево и вправо, соответственно.

; (1 2 3 4 5) -> (2 3 4 5 1)
(defun swap-to-left (list)
    (reduce 
        (lambda (tmp result)
            (append (list tmp) result)
        ) (cdr list) :initial-value (list (car list)) :from-end t
    )
)

; (1 2 3 4 5) -> (5 1 2 3 4)
(defun swap-to-right (list)
    (reduce 
        (lambda (result tmp)
            (if (equal (length result) (length list))
                result
                (append result (list tmp))
            )
        ) list :initial-value (last list)
    )
)

; ----------------------- ЗАДАНИЕ 4 --------------------------
; Напишите функцию, которая умножает на заданное
; число-аргумент все числа из заданного списка-аргумента, когда

; a. все элементы списка — числа,
; 6. элементы списка — любые объекты.

; умножение всех элементов числового списка на число
(defun mult-by (digit list)
    (mapcar 
        (lambda(x) 
            (* x digit)
        ) list)
)

; умножение произвольного списка на число 
(defun super-mult-by (digit list)
    (mapcar 
        (lambda(x) 
            (cond 
                ((numberp x) (* x digit))
                ((listp x) (super-mult-by digit x))
                (t x)
            )
        ) list
    )
)

; ----------------------- ЗАДАНИЕ 5 --------------------------
; Напишите функцию, select-between, которая из списка-аргумента,
; содержащего только числа, выбирает только те, которые 
; расположены между двумя указанными границами-аргументами 
; и возвращает их в виде списка


(defun select-between (left right list)
    (let ((lb (min left right)) (rb (max left right)))
        (reduce 
            (lambda (result tmp)
                (if (and (> tmp lb) (< tmp rb)) 
                    (append result (list tmp)) 
                    result
                )
            ) list :initial-value nil
        )
    )
)

(defun insert-elem (elem list)
    (cond 
        ( (null list)  (cons elem nil) )
        ( (< elem (car list))  (cons elem list) )
        ( t  (cons (car list) (insert-elem elem (cdr list))) )
    )
)

(defun my-sort (list)
    (reduce 
        (lambda (sorted tmp)
            (insert-elem tmp sorted)
        ) list :initial-value nil
    )
)

(defun super-select-between(left right list)
    (my-sort (select-between left right list))
)


; добавить элемент в конец списка
; не эффективно, видимо
(defun push-back (elem list)
    (cond 
        ((null (cdr list)) (cons (car list) (cons elem nil)))
        (t (cons (car list) (push-back elem (cdr list))))
    )
)

(push-back 5 '(1 2 3 4))

; добавляет в начало
(defun push-front (elem list)
    (cond 
        ((null list) (cons elem nil))
        (t (cons elem list))
    )
)

(push-front 5 '(1 2 3 4))