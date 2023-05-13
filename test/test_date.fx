/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// tests for Date module

from UTest import *
import Date

TEST("date.check_calendar", fun() {
    val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    var prevJ = -1
    for y0 <- -4712:3000 {
        for m0 <- 1:13 {
            var maxday = days[m0-1]
            if maxday == 28 && Date.is_leap_year(y0) {
                maxday += 1
            }
            for d0 <- 1:maxday+1 {
                val date = Date.make(y0, m0, d0)
                val idate = y0*10000 + m0*100 + d0
                if idate == 15821015 {
                    prevJ = 2299160
                }
                val (y1, m1, d1) = date.unpack()
                EXPECT_EQ(`date.jdn`, prevJ + 1)
                EXPECT_EQ(`(y0, m0, d0)`, `(y1, m1, d1)`)
                prevJ = date.jdn;
            }
        }
    }
})

TEST("date.regression", fun() {
    val date0 = Date.parse("1 Jan 1970")
    EXPECT_EQ(`date0.jdn`, 2440588)
    EXPECT_EQ(`date0.calendar`, Date.Calendar_Gregorian)
    EXPECT_EQ(`date0.weekday_english_name()`, "Thursday")
    EXPECT_EQ(`string(date0)`, "1970-01-01")

    EXPECT_EQ(`string(Date.parse("07/11/1917", "DD/MM/YYYY").rebase(Date.Calendar_Julian))`, "1917-10-25")

    EXPECT_EQ(`Date.is_leap_year(2000)`, true)
    EXPECT_EQ(`Date.is_leap_year(2012)`, true)
    EXPECT_EQ(`Date.is_leap_year(2100)`, false)
    EXPECT_EQ(`Date.is_leap_year(1970)`, false)

    EXPECT_EQ(`Date.parse("May 12, 2023").weeknumber()`, 19)

    val date1 = Date.parse("1452-04-15")
    EXPECT_EQ(`date1.jdn`, 2251506)
    EXPECT_EQ(`date1.calendar`, Date.Calendar_Julian)
    EXPECT_EQ(`date1.weekday()`, 6)
    EXPECT_EQ(`string(date1)`, "1452-04-15")
})
