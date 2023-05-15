/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Operations on dates.
    Each date is represented as a Julian day number (JDN):
    https://en.wikipedia.org/wiki/Julian_day.

    Briefly, Julian day number is a 0-based day
    counter since -4712-01-01 (1 Jan -4712 ~ JDN=0).
    The counter is continuous and almost monotonous.
    When using hybrid calendar (Calendar_Auto parameter),
    Dates from 5 Oct 1582 till 14 Oct 1582, inclusive,
    are repeated twice
    (i.e. they map to the same JDN as the next 10 days).
    The ambiguity is resolved in favor
    of Gregorian calendar. If a program operates with a time span
    that may include those 10 days, you may explicitly specify
    Calendar_Gregorian or Calendar_Julian instead of
    Calendar_Auto (in other words, use "proleptic Gregorian" or
    "proleptic Julian" calendar) to avoid (or introduce)
    possible confusion.

    The formulae for conversion between a date and JDN
    is taken from that Wikipedia article.

    Different calendars can be used when converting
    Year-Month-Day to/from JDN:

    * Calendar_Gregorian - Gregorian calendar
        (https://en.wikipedia.org/wiki/Gregorian_calendar).
        The modern calendar, currently used in most countries,
        officially and often unofficially. Essentially,
        Gregorian calendar is very similar to Julian calendar,
        with the same number of months and duration of each month,
        but the leap year formula is slightly more complex,
        see is_leap_year() implementation.
    * Calendar_Julian - Julian calendar
        (https://en.wikipedia.org/wiki/Julian_calendar),
        that was widespread in Europe till 14 Oct 1582 inclusive
        and was gradually replaced by Gregorian calendar.
    * Calendar_Auto - it's a concrete calandar.
        the conversion function will choose Gregorian or Julian,
        depending on the date:
        1) Gregorian for dates after 14 Oct 1582 (JDN >= 2299161)
        2) and Julian before that.
        Note that Calendar_Auto is not stored in the date,
        instead Gregorian or Julian will be stored.

    Support for other calendars is to be added later.
*/

@ccode {
#include <time.h>
}

exception DateError: string

type calendar_t =
    | Calendar_Auto
    | Calendar_Gregorian
    | Calendar_Julian

type monthfmt_t =
    | Month_Number
    | Month_Short
    | Month_Full

class t =
{
    jdn: int
    calendar: calendar_t
}

fun string(calendar: calendar_t): string
{
| Calendar_Auto => "Auto"
| Calendar_Gregorian => "Gregorian"
| Calendar_Julian => "Julian"
}

val MIN_YEAR = -4712

@private fun month_name_(month: int, shortname: bool): string
@ccode
{
    const int days[] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};
    char buf[64];
    if ((unsigned)(month-1) > 11U)
        FX_FAST_THROW_RET(FX_EXN_OutOfRangeError);
    {
    time_t ts = (time_t)(days[month-1]*24*60*60 + 12*60*60);
    struct tm* t = localtime(&ts);
    strftime(buf, 64, (shortname ? "%b" : "%B"), t);
    }
    return fx_cstr2str(buf, -1, fx_result);
}

@private fun weekday_name_(wd: int, shortname: bool): string
@ccode
{
    char buf[64];
    if ((unsigned)wd > 7U)
        FX_FAST_THROW_RET(FX_EXN_OutOfRangeError);
    {
        time_t ts = (time_t)((wd + 3)*24*60*60 + 12*60*60);
        struct tm* t = localtime(&ts);
        strftime(buf, 64, (shortname ? "%a" : "%A"), t);
    }
    return fx_cstr2str(buf, -1, fx_result);
}

val month_names_eng = ["January", "February", "March", "April",
    "May", "June", "July", "August", "September",
    "October", "November", "December"]
val month_short_names_eng = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
val month_names = [for i <- 1:13 {month_name_(i, false)}]
val month_short_names = [for i <- 1:13 {month_name_(i, true)}]
val month_names_eng_cap = [for m <- month_names_eng {m.toupper()}]
val month_short_names_eng_cap = [for m <- month_short_names_eng {m.toupper()}]
val month_names_cap = [for m <- month_names_eng {m.toupper()}]
val month_short_names_cap = [for m <- month_short_names_eng {m.toupper()}]

val weekday_names_eng = ["Sunday", "Monday", "Tuesday", "Wednesday",
    "Thursday", "Friday", "Saturday", "Sunday"]
val weekday_short_names_eng = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
val weekday_names = [for i <- 0:8 {weekday_name_(i, false)}]
val weekday_short_names = [for i <- 0:8 {weekday_name_(i, true)}]

operator + (date: t, delta: int) = date.{jdn = date.jdn + delta}
operator + (delta: int, date: t) = date.{jdn = date.jdn + delta}
operator - (date: t, delta: int) = date.{jdn = date.jdn - delta}
operator - (date1: t, date2: t) = date1.jdn - date2.jdn

operator == (date1: t, date2: t) = date1.jdn == date2.jdn
operator <=> (date1: t, date2: t) = date1.jdn <=> date2.jdn

fun is_leap_year(year: int, ~calendar: calendar_t=Calendar_Auto): bool
{
    if year % 4 != 0 {
        false
    } else if year < 1582 &&
        (calendar == Calendar_Auto || calendar == Calendar_Julian) {
        true
    } else if year % 100 != 0 {
        true
    } else {
        year % 400 == 0
    }
}

fun month_days(year: int, month: int, ~calendar: calendar_t=Calendar_Auto): int
{
    match month {
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
    | 4 | 6 | 9 | 11 => 30
    | 2 => 28 + int(is_leap_year(year, calendar=calendar))
    | _ => throw BadArgError
    }
}

fun final_weekday(year: int): int
{
    var y100 = year/100
    y100 -= int(year < y100*100)
    var y400 = year/400
    y400 -= int(year < y400*400)
    var p = (year + (year>>2) - y100 + y400) % 7
    p + int(p < 0)*7
}

fun number_of_weeks(year: int)
{
    if final_weekday(year) == 4 || final_weekday(year-1) == 3 {
        53
    } else {
        52
    }
}

fun make(year: int, month: int, day: int, ~calendar: calendar_t=Calendar_Auto)
{
    if year < MIN_YEAR {
        throw DateError(f"year value is out of range [{MIN_YEAR}, inf)")
    }
    if month <= 0 || month > 12 {
        throw DateError("month value is out of range [1, 12]")
    }
    val ndays = month_days(year, month, calendar=calendar)
    if day <= 0 || day > ndays {
        throw DateError(f"day value is out of range [1, {ndays}]")
    }

    if calendar == Calendar_Gregorian ||
       (calendar == Calendar_Auto && year*10000 + month*100 + day > 15821014) {
        val m = (month - 14)/12

        t { jdn = (year + 4800 + m)*1461/4 +
                (month - 2 - 12*m)*367/12 -
                ((year + 4900 + m)/100)*3/4 + day - 32075,
            calendar = Calendar_Gregorian }
    } else {
        t { jdn = 367*year - (year + 5001 + (month - 9)/7)*7/4 + month*275/9 + day + 1729777,
            calendar = Calendar_Julian }
    }
}

fun make(jdn: int, ~calendar: calendar_t=Calendar_Auto) {
    val calendar =
        if calendar != Calendar_Auto {calendar}
        else if jdn >= 2299161 {Calendar_Gregorian}
        else {Calendar_Julian}
    t {jdn = jdn, calendar=calendar}
}

fun make((year, month, day): (int*3), ~calendar: calendar_t=Calendar_Auto) =
    make(year, month, day, calendar=calendar)

// returns (year, month, day)
fun unpack(date: t): (int*3) {
    if date.jdn < 0 {
        throw DateError(f"invalid date (too far in the past): \
            date.jdn={date.jdn} is out of [0, inf) range")
    }
    var f = date.jdn + 1401
    if date.calendar == Calendar_Gregorian {
        f += (((date.jdn*4 + 274277)/146097)*3)/4 - 38;
    }
    val e = f*4 + 3
    val y = e / 1461
    val g = (e - y*1461)/4
    val h = g*5 + 2
    val m = h / 153
    val month = (m + 2) % 12 + 1
    (y - 4716 + (14 - month)/12, month, (h - m * 153)/5 + 1)
}

fun rebase(date: t, calendar: calendar_t): t
{
    if (date.calendar == calendar) {
        date
    } else {
        // [TODO] maybe need to shift JDN between if it's between 5 and 15 Oct 1582
        val jdn = date.jdn
        t {jdn = jdn, calendar = calendar}
    }
}

fun parse_month(s: string)
{
    val mstr = s.toupper()
    match find_opt(for i <- 0:12 {
        mstr == month_names_cap[i] ||
        mstr == month_short_names_cap[i] ||
        mstr == month_names_eng_cap[i] ||
        mstr == month_short_names_eng_cap[i]
        }) {
    | Some(idx) => idx + 1
    | _ => throw DateError(f"cannot recognize month '{s}'")
    }
}

fun parse(date: string, format: string, ~calendar: calendar_t=Calendar_Auto)
{
    val fmt = format.toupper()
    val ((yidx, have_century), (midx, mfmt), didx, sep1, sep2) = match fmt {
        | "YYYY-MM-DD" | "" => ((1, 1), (2, Month_Number), 3, '-', '-')
        | "MM/DD/YYYY" => ((3, 1), (1, Month_Number), 2, '/', '/')
        | "MM/DD/YY" => ((3, -1), (1, Month_Number), 2, '/', '/')
        | "MM/DD/YY(YY)" => ((3, 0), (1, Month_Number), 2, '/', '/')
        | "DD/MM/YYYY" => ((3, 1), (2, Month_Number), 1, '/', '/')
        | "DD/MM/YY" => ((3, -1), (2, Month_Number), 1, '/', '/')
        | "DD/MM/YY(YY)" => ((3, 0), (2, Month_Number), 1, '/', '/')
        | "DD.MM.YYYY" => ((3, 1), (2, Month_Number), 1, '.', '.')
        | "DD.MM.YY" => ((3, -1), (2, Month_Number), 1, '.', '.')
        | "DD.MM.YY(YY)" => ((3, 0), (2, Month_Number), 1, '.', '.')
        | "DD MMM YYYY" => ((3, 1), (2, Month_Short), 1, ' ', ' ')
        | "DD MMMM YYYY" => ((3, 1), (2, Month_Full), 1, ' ', ' ')
        | "MMMM DD YYYY" => ((3, 1), (1, Month_Full), 2, ' ', ',')
        | "YYYY年MM月DD日" => ((1, 1), (2, Month_Number), 3, '年', '月')
        | "YY(YY)年MM月DD日" => ((1, 0), (2, Month_Number), 3, '年', '月')
        | _ => throw DateError(f"unrecognized date format '{format}'")
        }
    val pos1 = date.find(sep1, 1)
    if pos1 < 0 {
        throw DateError(f"date separator '{sep1}' is not found")
    }
    var pos2 = date.find(sep2, pos1+1)
    if pos2 < 0 {
        throw DateError(f"date separator '{sep2}' is not found")
    }
    val part1 = date[:pos1].strip()
    val part2 = date[pos1+1:pos2].strip()
    while !date[pos2].isdigit() {
        pos2 += 1
    }
    var pos3 = pos2+1
    val datelen = date.length()
    while pos3 < datelen && date[pos3].isdigit() {
        pos3 += 1
    }
    val part3 = date[pos2:pos3].strip()
    val ystr = if yidx == 1 {part1} else if yidx == 2 {part2} else {part3}
    val mstr = if midx == 1 {part1} else if midx == 2 {part2} else {part3}
    val dstr = if didx == 1 {part1} else if didx == 2 {part2} else {part3}
    //println(f"y: {ystr}, m: {mstr}, d: {dstr}")
    var year = ystr.to_int_or(-5000, base=10)
    if year < MIN_YEAR {
        throw DateError(f"year value is out of range [{MIN_YEAR}, inf)")
    }
    if have_century < 0 || (have_century == 0 && ystr.length()==2) {
        year += if year >= 40 {1900} else {2000}
    }
    val month =
        if mfmt == Month_Number {
            val month = mstr.to_int_or(-1, base=10)
            if month < 1 || month > 12 {
                throw DateError(f"month value '{mstr}' is not a number or \
                                  is out of [1, 12] range")
            }
            month
        } else {
            parse_month(mstr)
        }
    val day = dstr.to_int_or(-1, base=10)
    if day < 1 || day > 31 {
        throw DateError(f"day value '{dstr}' is not a number or \
                          is out of [1, 31] range")
    }
    make(year, month, day, calendar=calendar)
}

fun recognize_format(date: string)
{
    var pos1 = date.find('-', 1)
    if pos1 > 0 {
        "YYYY-MM-DD"
    } else {
        pos1 = date.find('.', 1)
        if pos1 > 0 {
            "DD.MM.YY(YY)"
        } else {
            pos1 = date.find('/', 1)
            if pos1 > 0 {
                val part1 = date[:pos1]
                val m_or_d = part1.to_int_or(-1, base=10)
                if m_or_d > 12 {
                    "DD/MM/YY(YY)"
                } else if m_or_d > 0 {
                    "MM/DD/YY(YY)"
                } else {
                    throw DateError(f"the first part of date '{part1}' is not a number")
                }
            } else {
                pos1 = date.find('年', 1)
                if pos1 > 0 {
                    "YY(YY)年MM月DD日"
                } else {
                    pos1 = date.find(' ', 1)
                    if pos1 > 0 {
                        pos1 = date.find(',', 1)
                        if pos1 > 0 {
                            "MMMM DD YYYY"
                        } else {
                            "DD MMM YYYY"
                        }
                    } else {
                        throw DateError(f"the date '{date}' is not recognized. Please, specify the format explicitly")
                    }
                }
            }
        }
    }
}

fun parse(date: string, ~calendar: calendar_t = Calendar_Auto) =
    parse(date, recognize_format(date), calendar=calendar)

fun gregorian(year: int, month: int, day: int) =
    make(year, month, day, calendar=Calendar_Gregorian)

fun julian(year: int, month: int, day: int) =
    make(year, month, day, calendar=Calendar_Julian)

fun gregorian(date: string, format: string) =
    parse(date, format, calendar=Calendar_Gregorian)

fun julian(date: string, format: string) =
    parse(date, format, calendar=Calendar_Julian)

fun gregorian(date: string) =
    parse(date, calendar=Calendar_Gregorian)

fun julian(date: string) =
    parse(date, calendar=Calendar_Julian)

@private @pure @nothrow fun today_ymd_(): (int*3)
@ccode {
    time_t t = time(0);
    struct tm* lt = localtime(&t);
    int year = lt->tm_year + 1900;
    int month = lt->tm_mon + 1;
    int day = lt->tm_mday;
    fx_result->t0 = year;
    fx_result->t1 = month;
    fx_result->t2 = day;
}

fun today()
{
    val (y, m, d) = today_ymd_()
    make(y, m, d, calendar=Calendar_Auto)
}

fun weekday(date: t)
{
    val wday = date.jdn % 7
    wday + int(wday < 0)*7 + 1
}

fun weekday_name(date: t) = weekday_names[weekday(date)]
fun weekday_short_name(date: t) = weekday_short_names[weekday(date)]
fun weekday_english_name(date: t) = weekday_names_eng[weekday(date)]
fun weekday_short_english_name(date: t) = weekday_short_names_eng[weekday(date)]

fun month_name(month: int) = month_names[month-1]
fun month_short_name(month: int) = month_short_names[month-1]
fun month_english_name(month: int) = month_names_eng[month-1]
fun month_short_english_name(month: int) = month_short_names_eng[month-1]

fun month_name(date: t) = month_names[unpack(date).1-1]
fun month_short_name(date: t) = month_short_names[unpack(date).1-1]
fun month_english_name(date: t) = month_names_eng[unpack(date).1-1]
fun month_short_english_name(date: t) = month_short_names_eng[unpack(date).1-1]

@inline fun year(date: t) = unpack(date).0
@inline fun month(date: t) = unpack(date).1
@inline fun day(date: t) = unpack(date).2

@private @pure @nothrow
fun day_of_the_year_(leapyear: bool, month: int, day: int): int
@ccode {
    const int days[][12] = {
        {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334},
        {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335}
    };
    return days[leapyear][month-1] + day;
}

fun day_of_the_year(date: t)
{
    val (year, month, day) = unpack(date)
    val leapyear = is_leap_year(year, calendar=date.calendar)
    day_of_the_year_(leapyear, month, day)
}

/*
    returns week number within [1, 53] range, according to ISO 8601
    https://en.wikipedia.org/wiki/ISO_week_date
*/
fun weeknumber(date: t, ~weekstartsfrom: int=1)
{
    var date_ = date
    var wday = weekday(date)
    match weekstartsfrom {
    | 1 => {}
    | 0 | 7 =>
        if wday == 7 {date_ += 1; wday = 1}
    | 6 =>
        if wday == 0 || wday >= 6 {date_ += 2; wday = (wday + 2) % 7 }
    | _ =>
        throw DateError(f"invalid weekstartsfrom={weekstartsfrom}: \
            it should be Monday (1), Saturday (6) or Sunday (0 or 7)")
    }
    val (year, month, day) = unpack(date_)
    val leapyear = is_leap_year(year, calendar = date_.calendar)
    val doy = day_of_the_year_(leapyear, month, day)
    val wd = (doy - wday + 10)/7
    if wd < 1 {
        number_of_weeks(year-1)
    } else if wd > 52 {
        val nweeks = number_of_weeks(year)
        if wd <= nweeks {wd} else {1}
    } else {
        wd
    }
}

fun string(date: t)
{
    val (y,m,d) = unpack(date)
    f"{y:04d}-{m:02d}-{d:02d}"
}

// [TODO] provide more sophisticated date formatting via format(...)
// ...

// [TODO] provide conversion to/from "ticks" since some standard date, e.g. 1970-01-01
// ...
