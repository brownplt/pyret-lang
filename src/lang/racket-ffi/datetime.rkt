#lang pyret

provide {
  current-time: current-time,
  current-seconds: current-seconds,
  current-milliseconds: current-milliseconds,
  display-formats : display-formats,
} end

import Racket as R

fun EqSymbol(s :: String):
  b = brander()
  sym = R("racket/base")("string->symbol", s)
  b.brand({
      symbol: sym,
      _equals(self,other): b.test(self) and b.test(other) end,
      tostring(self): s end
    })
end

data DateDisplayFormat:
  | american
  | chinese
  | german
  | indian
  | irish
  | iso-8601-short
  | iso-8601-long
  | rfc2822
#  | julian
end

display-formats = {
  american: american,
  chinese: chinese,
  german: german,
  indian: indian,
  irish: irish,
  iso-8601-short: iso-8601-short,
  iso-8601-long: iso-8601-long,
  rfc2822: rfc2822,
  # julian: julian
}

fun pad-left(width, val, padding):
  val-str = tostring(val)
  if val-str.length() < width: padding.repeat(width - val-str.length()) + val-str
  else: val-str
  end
end

fun current-seconds():
  R("racket/base")("current-seconds")
end

fun current-milliseconds():
  R("racket/base")("current-milliseconds")
end

fun current-time():
  cur-seconds = current-seconds()
  time = R("racket/base")("seconds->date", cur-seconds)
  second = R("racket/base")("date-second", time)
  minute = R("racket/base")("date-minute", time)
  hour = R("racket/base")("date-hour", time)
  day = R("racket/base")("date-day", time)
  month = R("racket/base")("date-month", time)
  year = R("racket/base")("date-year", time)
  week-day = R("racket/base")("date-week-day", time)
  year-day = R("racket/base")("date-year-day", time)
  is-dst = R("racket/base")("date-dst?", time)
  time-zone-offset = R("racket/base")("date-time-zone-offset", time)
  date(second, minute, hour, day, month, year, week-day, year-day, is-dst, time-zone-offset)
end

english-months = [
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
]

german-months = [
  "Januar", "Februar", "M\344rch", "April", "May", "Juni",
  "Juli", "August", "September", "Oktober", "November", "Dezember"
]

english-days = [ "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" ]

chinese-days = [ "\u5929", "\u4E00", "\u4E8C", "\u4e09", "\u56DB", "\u4E94", "\u516D" ]

fun month-number-to-string(month :: Number, months) -> String:
  if (month >= 1) and (month <= 12): months.get(month - 1)
  else: ""
  end
end

fun day-number-to-string(day :: Number, days) -> String:
  if (day >= 0) and (day <= 6): days.get(day)
  else: ""
  end
end

data Date:
  | date(second :: Number,
      minute :: Number,
      hour :: Number,
      day :: Number,
      month :: Number,
      year :: Number,
      week-day :: Number,
      year-day :: Number,
      is-dst :: Bool,
      time-zone-offset :: Number)
sharing:
  tostring(self): self.format(iso-8601-long, true) end,
  format(self, fmt :: DateDisplayFormat, show-time :: Bool):
    year = pad-left(4, self.year, "0")
    num-month = tostring(self.month)
    day = tostring(self.day)
    day-th =
      if (11 <= self.day) and (self.day <= 13): "th"
      else:
        mod = self.day.modulo(10)
        if mod == 1: "st"
        else if mod == 2: "nd"
        else if mod == 3: "rd"
        else: "th"
        end
      end
    am-pm = if self.hour >= 12: "pm" else: "am" end
    hour24 = pad-left(2, self.hour, "0")
    hour12 =
      if self.hour == 0: "12"
      else if self.hour > 12: tostring(self.hour - 12)
      else: tostring(self.hour)
      end
    day-string = cases(DateDisplayFormat) fmt:
      | american =>
        day-number-to-string(self.week-day, english-days) + ", "
          + month-number-to-string(self.month, english-months) + " "
          + day + day-th + ", " + year
      | chinese =>
        year + "/" + num-month + "/" + day + "\u661F\u671F" + day-number-to-string(self.week-day, chinese-days)
      | indian =>
        day + "-" + num-month + "-" + year
      | german =>
        day + ". " + month-number-to-string(self.month, german-months) + " " + year
      | irish =>
        day-number-to-string(self.week-day, english-days) + ", " + day + day-th + " "
          + month-number-to-string(self.month, english-months) + " " + year
      # | julian =>
      | iso-8601-short => year + pad-left(2, self.month, "0") + pad-left(2, self.day, "0")
      | iso-8601-long =>year + "-" + pad-left(2, self.month, "0") + "-" + pad-left(2, self.day, "0")
      | rfc2822 =>
        day-number-to-string(self.week-day, english-days).substring(0, 3) + ", " + day
          + " " + month-number-to-string(self.month, english-months).substring(0, 3) + " " + year
    end
    if not show-time: day-string
    else:
      neg = self.time-zone-offset < 0
      tz = if neg: 0 - self.time-zone-offset else: self.time-zone-offset end
      tz-sec = tz.modulo(60)
      tz-min = ((tz - tz-sec) / 60).modulo(60)
      tz-hour = ((tz - (60 * tz-min) - tz-sec) / 3600).modulo(60)
      sign =
        if neg: "-"
        else: "+"
        end
      day-string + cases(DateDisplayFormat) fmt:
        | american =>
          " " + hour12 + ":" + pad-left(2, self.minute, "0") + ":" + pad-left(2, self.second, "0") + am-pm
        | chinese => " " + hour24 + ":" + pad-left(2, self.minute, "0") + ":" + pad-left(2, self.second, "0")
        | indian =>
          " " + hour12 + ":" + pad-left(2, self.minute, "0") + ":" + pad-left(2, self.second, "0") + am-pm
        | german => ", " + hour24 + "." + pad-left(2, self.minute, "0")
        | irish => ", " + hour12 + ":" + pad-left(2, self.minute, "0") + am-pm
        # | julian
        | iso-8601-short =>
          "T" + pad-left(2, self.hour, "0") + pad-left(2, self.minute, "0") + pad-left(2, self.second, "0")
            + sign + pad-left(2, tz-hour, "0") + pad-left(2, tz-min, "0")
        | iso-8601-long =>
          "T" + pad-left(2, self.hour, "0") + ":" + pad-left(2, self.minute, "0")
            + ":" + pad-left(2, self.second, "0")
            + sign + pad-left(2, tz-hour, "0") + ":" + pad-left(2, tz-min, "0")
        | rfc2822 =>
          sign + pad-left(2, tz-hour, "0") + pad-left(2, tz-min, "0")
      end
    end
  end
end
