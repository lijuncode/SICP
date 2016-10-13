
;1.1: 各个表达式的结果：
;10
;12
;8
;6
;3
;4
;19
;4
;16
;6
;16


;1.2:
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) 
   (* 3 (- 6 2) (- 2 7)))


;1.3请定义一个过程，它以三个数为参数，返回其中较大的两个数之和：
(define (max-sum x y z)
  (cond ((and (not (> x y)) (not (> x z))) (+ y z))
        ((and (not (> y x)) (not (> y z))) (+ x z))
    	((and (not (> z y)) (not (> z x))) (+ y x))))

;1.4 求a + 正b的值，如果b>0，则a+b，否则 a-b。

;1.5 不论是应用序求值还是正则序求值，结果都是0呀，
;因为谓词部分先求值，(= 0 0)为真，接下来就直接返回0了

;以上是1.5最早的答案，后来发现有误，因为应用序求值是现求出参数的值，再去计算，而正则序求值则是先展开而后求值。
;所以如果是应用序求值，解释器执行`(test 0 (p)`时，会先求值(p)，这时就会陷入死循环。
;而如果是正则序求值，解释器就会先展开`test`，变成if表达式，然后再求值，因为谓词为真，就会直接返回0


;1.6： 程序会报错退出，具体原因是因为if的特殊性，if会先求值谓词，而普通的定义，会先求值参数，这样也会无限递归中。

;1.7：另一种策略：

(define (square x)
  (* x x))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (improve-good-enough? new-guess guess)
  (< (/ (abs (- new-guess guess)) guess) 0.0000001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (improve-sqrt-iter guess x)
  (if (improve-good-enough? (improve guess x) guess)
      guess
      (improve-sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (improve-sqrt-iter 1.0 x))


;1.8 求立方根：

(define (improve-good-enough? new-guess guess)
  (< (/ (abs (- new-guess guess)) guess) 0.0000001))

(define (improve-cube-root guess x)
  (/ (+ (/ x (square guess)) (* guess 2)) 3))

(define (cube-iter guess x)
  (if (improve-good-enough? (improve-cube-root guess x) guess)
      guess
      (cube-iter (improve-cube-root guess x) x)))

(define (cube-root x)
  (cube-iter 1.0 x))

(sqrt 0.00009)


