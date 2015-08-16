
(define zero?
  (lambda (n)
    (= n 0)))

(define sub
  (lambda (n)
    (- n 1)))

(define add
  (lambda (n)
    (+ n 1)))

;;;要求されたサイズでボード表現列を初期化する
(define mk-line
  (lambda (n)
    (cond
      ((zero? n) '())
      (else
        (cons
          -1
          (mk-line (sub n)))))))

;;;ボードを要求されたサイズで初期化する    
(define mk-board
  (lambda (col row)
    (cond
      ((zero? row) '())
      (else
        (cons
          (mk-line col)
          (mk-board col (sub row)))))))

;;; 配列内のデータをピックアップする
(define pick
  (lambda (lat n)
    (cond
      ((null? lat) '())
      ((zero? n) (car lat))
      (else
        (pick (cdr lat) (sub n))))))        

;;; 指定した座標の情報を取得する
(define pick-board
  (lambda (col row board)
    (cond
      ((null? board) '())
      (else
        (pick (pick board row) col)))))

;;; 指定した座標にデータを挿入する
(define update-board
  (lambda (board col row n)
    (cond
      ((zero? row)
        (cons
          (update-list (car board) col n)
          (cdr board)))
      (else
        (cons (car board)
          (update-board (cdr board) col (sub row) n))))))

(define update-list
  (lambda (lat col n)
    (cond
      ((zero? col) (cons n (cdr lat)))
      (else
        (cons (car lat)
          (update-list (cdr lat) (sub col) n))))))


;;; リストのサイズを取得する
(define lat-size
  (lambda (lat)
    (cond
      ((null? (cdr lat)) 0)
      (else
        (add (lat-size (cdr lat)))))))

;;; ボードの一行分を表現した文字列を返却する
(define draw-line-string
  (lambda (lat)
    (cond
      ((null? lat) "\n")
      (else
        (string-append (player-letter (car lat)) " " (draw-line-string (cdr lat)))))))

;;; プレイヤー情報を文字表現にする
(define player-letter
  (lambda (n)
    (cond
      ((not (number? n)) "?")
      (else
        (cond
          ((= -1 n) "+")
          ((= 0 n) "O")
          ((= 1 n) "@")
          (else "?"))))))

;;; ボードを標準出力に出力する
(define print-board
  (lambda (board)
    (cond
      ((null? board) (print ""))
      (else
        (print (draw-line-string (car board)))
        (print-board (cdr board))))))

;;; *********************************************************
(define *board-size* 9)
(define *board* (mk-board *board-size* *board-size*))

(print-board *board*)
(define *board* (update-board *board* 3 3 1))
(define *board* (update-board *board* 4 3 0))
(define *board* (update-board *board* 5 3 1))
(define *board* (update-board *board* 6 3 0))
(print-board *board*)


