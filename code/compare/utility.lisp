(in-package #:quaviver/compare)

(defun run-program-capture (command &key directory)
  "Run a program and capture the output as a string."
  (ignore-errors
   (multiple-value-bind (standard-output error-output code)
       (uiop:run-program command
                         :directory directory
                         :ignore-error-status t
                         :output '(:string :stripped t)
                         :error-output '(:string :stripped t))
     (declare (ignore error-output))
     (when (zerop code)
       standard-output))))

(defun cpu-count ()
  (loop for command in #+bsd '("sysctl -n hw.physicalcpu"
                               "sysctl -n hw.ncpu"
                               "sysctl -n hw.ncpufound")
                       #-bsd '("nproc --all")
        for output = (run-program-capture command)
        when output
          return (parse-integer output :junk-allowed t)))

(defun internal-time (stream value colon at &rest args)
  (declare (ignore colon at args))
  (multiple-value-bind (minutes seconds)
      (floor (floor value internal-time-units-per-second)
             60)
    (multiple-value-bind (hours minutes)
        (floor minutes 60)
      (format stream "~d:~2,'0d:~2,'0d"
              hours minutes seconds))))

(defun float-hex-digits (float-type)
  (ceiling (quaviver:storage-size float-type) 4))
