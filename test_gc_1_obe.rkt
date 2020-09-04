;; This program can run on a minimum of 176 bytes since the the value taken on by x is repeatedly garbage collected and reused.
(for x in (..= 1 1000000 1) do
	(println x))
