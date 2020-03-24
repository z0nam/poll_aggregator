cd "/Users/j/Documents/writings/2020/20200303_poll_aggregater/_analysis/_cho/"

clear all

insheet using "/Users/j/Documents/writings/2020/20200303_poll_aggregater/_analysis/_cho/result_data3_sigma2.csv"

drop v1

rename date date_str
gen date = date(date_str, "YMD")
format date %tdCCYY-NN-DD
drop date_str

collapse (sum) win_prob, by(date party)
rename win_prob expected_seats_129
