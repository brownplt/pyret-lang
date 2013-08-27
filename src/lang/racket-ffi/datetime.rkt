#lang pyret

import racket as R
provide {
  current-time: current-time,
  current-seconds: current-seconds,
  current-milliseconds: current-milliseconds,
  display-formats : display-formats,
} end

fun Eq(v):
  b = brander()
  b.brand({symbol: v, _equals(self,other): b.test(self) and b.test(other) end})
end

display-formats = {
  # american: Eq(R("racket/base")("string->symbol", "american")),
  # chinese: Eq(R("racket/base")("string->symbol", "chinese")),
  # german: Eq(R("racket/base")("string->symbol", "german")),
  # indian: Eq(R("racket/base")("string->symbol", "indian")),
  # irish: Eq(R("racket/base")("string->symbol", "irish")),
  iso-8601: Eq(R("racket/base")("string->symbol", "iso-8601")),
  readable-iso-8601: Eq(R("racket/base")("string->symbol", "readable-iso-8601")),
  # rfc2822: Eq(R("racket/base")("string->symbol", "rfc2822")),
  # julian: Eq(R("racket/base")("string->symbol", "julian"))
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
  tostring(self): self.format(display-formats.iso-8601) end,
  format(self, fmt):
    tz-sec = self.time-zone-offset.modulo(60)
    tz-min = ((self.time-zone-offset - tz-sec)/60).modulo(60)
    tz-hour = ((self.time-zone-offset - (60*tz-min) - tz-sec)/60).modulo(60)
    sign =
      if tz-hour > 0: "+"
      else: "-"
      end
    tz-hour-abs =
      if tz-hour > 0: tz-hour
      else: 0 - tz-hour
      end
    if fmt == display-formats.iso-8601:
      pad-left(4, self.year, "0") + pad-left(2, self.month, "0") + pad-left(2, self.day, "0")
        + "T"
        + pad-left(2, self.hour, "0") + pad-left(2, self.minute, "0") + pad-left(2, self.second, "0")
        + sign
        + pad-left(2, tz-hour-abs, "0") + pad-left(2, tz-min, "0")
    else if fmt == display-formats.readable-iso-8601:
      pad-left(4, self.year, "0") + "-" + pad-left(2, self.month, "0") + "-" + pad-left(2, self.day, "0")
        + "T"
        + pad-left(2, self.hour, "0") + ":" + pad-left(2, self.minute, "0") + ":" + pad-left(2, self.second, "0")
        + sign
        + pad-left(2, tz-hour-abs, "0") + ":" + pad-left(2, tz-min, "0")
    else:
      "unknown format"
    end
  end
end
