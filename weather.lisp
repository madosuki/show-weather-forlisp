(ql:quickload 'dexador)
(ql:quickload 'cl-json)

(load "decode_unixtime")

(defvar *baseurl* "https://api.openweathermap.org/data/2.5/")
(defvar *api-list* '((current . "weather?") (5day . "forecast?")))
(defvar *appkey* "")

(defmacro putstr (s)
  `(progn (princ ,s)
          (fresh-line)))

(defmacro getvalue (key lst)
  `(cdr (assoc ,key ,lst)))

(defun get-weather-data (city country temp-specie &optional (current t))
  (let ((r nil))
    (if current
      (progn (setq r (dex:post (concatenate 'string *baseurl*
                                     (getvalue 'current *api-list*)
                                     "q=" city "," country
                                     "&appid=" *appkey*
                                     "&units=" temp-specie))))
      (progn (setq r (dex:post (concatenate 'string *baseurl*
                                     (getvalue '5day *api-list*)
                                     "q=" city "," country
                                     "&appid=" *appkey*
                                     "&units=" temp-specie)))))
    r))

(defmacro decode-json (s)
  `(json:decode-json-from-string ,s))

(defmacro get-value-from-json (key top l)
  `(cdr (assoc ,key (cdr (assoc ,top ,l)))))

(defmacro get-weather-from-json (key top l)
  `(cdr (assoc ,key (cadr (assoc ,top ,l)))))

(defstruct weather-struct
  (weather nil)
  (description nil)
  (icon nil))

(defstruct coord-struct
  (longitude nil)
  (latitude nil))

(defstruct temperture-struct
  (temperture nil)
  (pressure nil)
  (humidity nil)
  (min-temperture nil)
  (max-temperture nil))

(defstruct wind-struct
  (speed nil)
  (degree nil))

(defstruct sunrise-struct
  (sunrise nil)
  (sunset nil))

(defvar *cloudiness* nil)
(defvar *rain-precipitation* nil)

(defvar *weather-param* (make-weather-struct))
(defvar *coord-param* (make-coord-struct))
(defvar *temperture-param* (make-temperture-struct))
(defvar *wind-param* (make-wind-struct))
(defvar *sun-param* (make-sunrise-struct))

(defmacro get-weather ()
  `(weather-struct-weather *weather-param*))
(defmacro get-description ()
  `(weather-struct-description *weather-param*))

(defmacro get-latitude ()
  `(coord-struct-latitude *coord-param*))
(defmacro get-lognitude ()
  `(coord-struct-longitude *coord-param*))

(defmacro get-temperture ()
  `(temperture-struct-temperture *temperture-param*))
(defmacro get-pressure ()
  `(temperture-struct-pressure *temperture-param*))
(defmacro get-humidity ()
  `(temperture-struct-humidity *temperture-param*))
(defmacro get-min-temp ()
  `(temperture-struct-min-temperture *temperture-param*))
(defmacro get-max-temp ()
  `(temperture-struct-max-temperture *temperture-param*))

(defmacro get-sunrise ()
  `(sunrise-struct-sunrise *sun-param*))
(defmacro get-sunset ()
  `(sunrise-struct-sunset *sun-param*))

(defmacro get-wind-speed ()
  `(wind-struct-speed *wind-param*))
(defmacro get-wind-direction ()
  `(wind-struct-degree *wind-param*))

(defmacro parse-coord (v l)
  `(get-value-from-json ,v ':coord ,l))

(defmacro parse-weather (v l)
  `(get-weather-from-json ,v ':weather ,l))

(defmacro parse-temperture (v l)
  `(get-value-from-json ,v ':main ,l))

(defmacro parse-wind (v l)
  `(get-value-from-json ,v ':wind ,l))

(defmacro parse-sun (v l)
  `(get-value-from-json ,v ':sys ,l))

(defun init-coord (json)
  (setf (get-lognitude) (parse-coord ':lon json))
  (setf (get-latitude) (parse-coord ':lat json)))

(defun init-weather (json)
  (setf (get-weather) (parse-weather ':main json))
  (setf (get-description) (parse-weather ':description json)))

(defun init-temperture (json)
  (setf (get-temperture) (parse-temperture ':temp json))
  (setf (get-pressure) (parse-temperture ':pressure json))
  (setf (get-humidity) (parse-temperture ':humidity json))
  (setf (get-min-temp) (parse-temperture ':temp--min json))
  (setf (get-max-temp) (parse-temperture ':temp--max json)))

(defun init-wind (json)
  (setf (get-wind-speed) (parse-wind ':speed json))
  (setf (get-wind-direction) (parse-wind ':deg json)))

(defun init-sun (json)
  (setf (get-sunrise) (parse-sun ':sunrise json))
  (setf (get-sunset) (parse-sun ':sunset json)))

(defmacro show-weather (country city weather description temp max-temp min-temp pressure
                        humidity lon lat speed deg vis sunrise sunset)
  `(format t "国: ~a 都市: ~a~%天気: ~a~%天気詳細: ~a~%気温: ~a~%最高気温: ~a 最低気温: ~a~%気圧: ~ahPa~%湿度: ~a%~%緯度: ~a 経度:~a~%風速: ~am/sec 風向き:~a~%視界: ~dm~%日の出: ~a, 日没: ~a~%"
           ,country ,city ,weather ,description ,temp ,max-temp ,min-temp
           ,pressure ,humidity ,lat ,lon ,speed ,deg ,vis ,sunrise ,sunset))

(defun run-process (country city)
  (let ((json (decode-json (get-weather-data city country "metric")))
        (sunrise nil)
        (sunset nil))
    (init-coord json)
    (init-weather json)
    (init-temperture json)
    (init-wind json)
    (init-sun json)
    
    (setq sunrise (decode-unixtime (get-sunrise)))
    (setq sunset (decode-unixtime (get-sunset)))
    
    (show-weather
     country city
     (get-weather) (get-description)
     (get-temperture) (get-max-temp) (get-min-temp) (get-pressure) (get-humidity)
     (get-latitude) (get-lognitude)
     (get-wind-speed) (get-wind-direction)
     (getvalue ':visibility json)
     sunrise sunset)))
    
    ;; (putstr json)))

(defun ask ()
  (let ((country nil)
        (city nil))
    (labels ((input-city (country)
               (format t "(1) Tokyo (2) Nagoya (3) Osaka (4) Fukuoka ?~%")
               (let ((num (read-line)))
                 (cond ((string= num "1")
                        (setq city "tokyo"))
                       ((string= num "2")
                        (setq city "nagoya"))
                       ((string= num "3")
                        (setq city "osaka"))
                       ((string= num "4")
                        (setq city "fukuoka"))
                       ((null (numberp (read-from-string num)))
                        (format t "Please Input Number.")
                        (input-city country)))
                 (run-process country city))))
      (format t "Please Input Country Name.~%")
      (setq country (read-line))
      (cond ((or (string= country "japan") (string= country "Japan") (string= country "日本"))
             (input-city country))
            ((string= country "q")
             (exit))
            (t (ask))))))

(defun main()
  (ask))

(main)

;; (let ((json (decode-json (get-weather-data "tokyo" "japan" "metric" nil))))
;;   (putstr (cddr (cadddr json))))
