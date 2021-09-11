fun get_nth_poly (xs : 'a list, n : int) =
	if n = 1
	then hd xs
	else get_nth_poly (tl xs, n - 1)

val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun purge (xs : int list) =
	let fun delete (el : int, xs : int list) =
		if null xs then [] else
			let val tail = delete (el, tl xs)
			in if hd xs = el then tail else hd xs :: tail end
	in if null xs then [] else
		hd xs :: purge (delete (hd xs, tl xs))
	end

fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
	number_in_months (dates, purge months)

fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
	dates_in_months (dates, purge months)

fun reasonable_date (date : int * int * int) =
	#1 date > 0 andalso
	#2 date > 0 andalso
	#2 date <= 12 andalso
	#3 date > 0 andalso
	#3 date <= (
		if #2 date = 2 andalso #1 date mod 4 = 0 andalso
			(#1 date mod 100 > 0 orelse #1 date mod 400 = 0)
		then 29
		else get_nth_poly (month_days, #2 date))
