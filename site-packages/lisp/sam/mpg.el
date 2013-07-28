;;;###autoload
(defun mpg (distance gas)
  "Calculate miles per gallon from kms and litres"
  (interactive "nDistance: \nnLitres: ")
  (message "distance %S gas %S" distance gas)
  (let* ((miles (* distance 0.621))
	 (gallons (* gas 0.22))
	 (us (* gas 0.264))
	 (mpg (/ miles gallons))
	 (lper100k  (/ (* gas 100.0) distance))
	 (kpg (/ distance gallons)))
    (message "%.0f mpg %.0f US %.1f l/100km (km/g %.0f)"
	     mpg (/ miles us) lper100k kpg)))

;;;###autoload
(defun gas-price-gallons (price)
  "Convert from litres to US gallons."
  (interactive "nPrice/l: ")
  (message "$%S/l = $%.2f/us gallon" price (* price 3.785)))

;;;###autoload
(defun gas-price-litres (price)
  "Convert from US gallons to litres."
  (interactive "nPrice/g: ")
  (message "$%S/l = $%.2f/us gallon" price (* price 0.264)))

;;;###autoload
(defun gas-estimate(distance economy gas)
  "Estimate the gas used in dollars."
  (interactive "nDistance: \nnL/100km: \nnGas Price (l): ")
  (message "%.02f" (* distance economy 0.01 gas)))

(provide 'mpg)
