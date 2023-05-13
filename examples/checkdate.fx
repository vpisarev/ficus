/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Sys, Date

val ERRVAL = -5000

fun try_parse_date_or_jdn(date: string) =
    try {
        Some(Date.parse(date))
    } catch {
    | Date.DateError(msg) =>
        val jdn = date.to_int_or(ERRVAL)
        if jdn == ERRVAL {
            println(f"Error: cannot recognize date '{date}': please, specify correct date or JDN (non-negative integer)")
            None
        } else {
            Some(Date.make(jdn))
        }
    | e =>
        println(f"Error: exception {e} occured when parsing '{date}'")
        None
    }

fun parse(idx: int, date0: string) {
    val date_ = date0.tolower()
    val some_date =
        if date_ == "today" || date_ == "now" {
            Some(Date.today())
        } else {
            try_parse_date_or_jdn(date0)
        }
    match some_date {
    | Some(date) =>
        println(f"Input #{idx}: '{date0}'")
        println(f"   Parsed date: {date}, {date.weekday_name()}, week {date.weeknumber():02d}")
        /*val gdate_str = try {
            string()
        } catch {
        | Date.DateError _ => "out of range"
        }*/
        println(f"   Gregorian: {date.rebase(Date.Calendar_Gregorian)}")
        println(f"   Julian: {date.rebase(Date.Calendar_Julian)}")
        println(f"   JDN: {date.jdn}")
    | _ => {}
    }
    some_date
}

val args = Sys.arguments()
if args.empty() {
    println("Usage: checkdate date1 ...")
} else {
    val dates = [:: for d@i <- args {parse(i+1, d)}]
    match dates {
    | Some(date1) :: Some(date2) :: [] =>
        println(f"Number of days between '{date1}' and '{date2}': {abs(date1 - date2)}")
    | _ => {}
    }
}
