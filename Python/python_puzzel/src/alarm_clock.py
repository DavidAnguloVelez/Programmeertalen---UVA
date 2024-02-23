import time

from gtts import gTTS
import playsound

HOURS_IN_DAY = 24
MINUTES_IN_HOUR = 60
SECONDS_IN_MINUTE = 60


class Time:
    """ Represents a time of day."""

    def __init__(self, hours, minutes, seconds):
        """ Initialises a Time object with integers 'hours', 'minutes' and
        'seconds.
        >>> t = Time(18, 30, 0)
        """

        self.time = hours * 3600 + minutes * 60 + seconds

        self.overflow_time()

    def __repr__(self):
        """ Returns the string representation of a Time object.
        >>> print( Time(8,5,30) )
        08:05:30
        """

        hours = self.get_hours()
        minutes = self.get_minutes()
        seconds = self.get_seconds()

        return f"{hours:02}:{minutes:02}:{seconds:02}"

    def overflow_time(self):
        self.time %= (24 * 3600)

    def get_hours(self):
        """ Returns the hours of the Time object.
        >>> Time(23,0,0).get_hours()
        23
        """

        return divmod(self.time, 3600)[0]

    def get_minutes(self):
        """ Returns the minutes of the Time object.
        >>> Time(0,59,0).get_minutes()
        59
        """

        return divmod(self.time, 60)[0] - self.get_hours() * 60

    def get_seconds(self):
        """ Returns the seconds of the Time object.
        >>> Time(0,0,59).get_seconds()
        59
        """

        return self.time - self.get_hours() * 3600 - self.get_minutes() * 60

    def set_time(self, hours, minutes, seconds):
        """ Sets the time of the Time object to 'hours', 'minutes',
        and 'seconds' making sure the values are in valid range:
          hours:   [0, HOURS_IN_DAY)
          minutes: [0, MINUTES_IN_HOUR)
          seconds: [0, SECONDS_IN_MINUTE)
        >>> time = Time(0, 0, 0)
        >>> time.set_time(0, 0, 90)
        >>> print(time)
        00:01:30
        >>> time.set_time(0, 0, 3600)
        >>> print(time)
        01:00:00
        >>> time.set_time(0, 0, -1)
        >>> print(time)
        23:59:59
        >>> time.set_time(10, -121, 0)
        >>> print(time)
        07:59:00
        >>> time.set_time(-50, 0, 0)
        >>> print(time)
        22:00:00
        >>> print(Time(10, -120, -150)) # __init__() test
        07:57:30
        """

        self.time = seconds + minutes * 60 + hours * 3600

        self.overflow_time()

        return

    def get_total_seconds(self):
        """ Returns the number of seconds since time 00:00:00.
        >>> Time(0,0,1).get_total_seconds()
        1
        >>> Time(0,1,0).get_total_seconds()
        60
        >>> Time(1,0,0).get_total_seconds()
        3600
        >>> Time(13,30,5).get_total_seconds()
        48605
        """

        return self.time

    def __add__(self, other):
        """ Returns a valid Time objects which is Time objects
        'other' added to 'self'.
        >>> print(Time(0,0,0) + Time(1,2,3))
        01:02:03
        >>> print(Time(13,30,0) + Time(1,46,-45))
        15:15:15
        """

        total_added_seconds = self.time + other.time

        return Time(0, 0, total_added_seconds)

    def __sub__(self, other):
        """ Returns a valid Time objects which is Time objects
        'other' substracted from 'self'.
        >>> print(Time(10,10,10) - Time(1,2,3))
        09:08:07
        >>> print(Time(10,0,0) - Time(1,50,600))
        08:00:00
        """

        total_subtracted_seconds = self.time - other.time

        return Time(0, 0, total_subtracted_seconds)


class Event:
    """ Represents an event that happens at a certain time."""

    def __init__(self, time, description):
        """ Initialises an Event object with a 'time' object of type Time and a
        'description' of type str.
        >>> event = Event(Time(18, 30, 0), "dinner")
        """

        self.time = time
        self.description = description

    def __repr__(self):
        """ Returns the string representation of an Event object.
        >>> print( Event(Time(18, 30, 0), "dinner") )
        18:30:00 dinner
        """

        return f"{self.time} {self.description}"

    def get_time(self):
        """ Returns the time of an Event object.
        >>> print( Event(Time(18, 30, 0), "dinner").get_time() )
        18:30:00
        """

        return self.time

    def get_description(self):
        """ Returns the description of an Event object.
        >>> print( Event(Time(18, 30, 0), "dinner").get_description() )
        dinner
        """

        return self.description


class AlarmClock:
    """ Represents an alarm clock that can handle events. """

    def __init__(self):
        """ Initialises an AlarmClock object with an empty list of events.
        >>> alarm_clock = AlarmClock()
        """

        self.events = []

    def add_event(self, event):
        """ Adds 'event' to this AlarmClock object, it doesn't return anything.
        >>> alarm_clock = AlarmClock()
        >>> event = Event(Time(18, 30, 0), "dinner")
        >>> alarm_clock.add_event(event)
        """

        self.events.append(event)
        self.sort()

    def __repr__(self):
        """ Returns a string representation of the AlarmClock object.
        >>> alarm_clock = AlarmClock()
        >>> event = Event(Time(18, 30, 0), "dinner")
        >>> alarm_clock.add_event(event)
        >>> s = str(alarm_clock)
        >>> "18:30:00" in s
        True
        >>> "dinner" in s
        True
        >>> "breakfast" in s
        False
        """

        return f"{self.events}"

    def __len__(self):
        """ Returns the number of events in this AlarmClock object.
        >>> alarm_clock = AlarmClock()
        >>> len(alarm_clock)
        0
        >>> event = Event(Time(18, 30, 0), "dinner")
        >>> alarm_clock.add_event(event)
        >>> len(alarm_clock)
        1
        """

        return (len(self.events))

    def sort(self):
        """ Sorts the events by time.
        >>> alarm_clock = AlarmClock()
        >>> alarm_clock.add_event( Event(Time(0, 0, 2), "event2") )
        >>> alarm_clock.add_event( Event(Time(0, 0, 1), "event1") )
        >>> s = str(alarm_clock)
        >>> s.find("event1") < s.find("event2")
        True
        """

        self.events.sort(key =
                         lambda event: event.get_time().get_total_seconds())

    def get_next_event(self):
        """ Returns the next event with the smallest time.
        >>> alarm_clock = AlarmClock()
        >>> alarm_clock.add_event( Event(Time(0, 0, 2), "event2") )
        >>> alarm_clock.get_next_event().get_description()
        'event2'
        >>> alarm_clock.add_event( Event(Time(0, 0, 1), "event1") )
        >>> alarm_clock.get_next_event().get_description()
        'event1'
        """

        return self.events[0]

    def remove_next_event(self):
        """ Removes and returns the next event with the smallest time.
        >>> alarm_clock = AlarmClock()
        >>> alarm_clock.add_event( Event(Time(0, 0, 2), "event2") )
        >>> alarm_clock.add_event( Event(Time(0, 0, 1), "event1") )
        >>> alarm_clock.remove_next_event().get_description()
        'event1'
        >>> alarm_clock.remove_next_event().get_description()
        'event2'
        """

        return self.events.pop(0)

    def wait_for_and_handle_events(self, event_handler):
        """ Wait for each event to pass and then print the event. """

        while self.events:
            seconds_till_event = (self.events[0].get_time()
                - now()).get_total_seconds()
            time.sleep(seconds_till_event)

            event_handler(self.remove_next_event())
            # Prints event happening and removes the event from the list.
            # print("ALARM:", self.events[0].get_time(),
            #        self.events[0].get_description())
            # text_to_speech(self.remove_next_event().get_description())

        return


def text_to_speech(text):
    tts = gTTS(text=text, lang='en')
    filename = "speech.mp3"
    tts.save(filename)
    playsound.playsound(filename, True)


def alarm_only_print(event):
    print("ALARM:", event.get_time(), event.get_description())


def alarm_only_tts(event):
    text_to_speech(event.get_description())


def alarm_print_and_tts(event):
    print("ALARM:", event.get_time(), event.get_description())
    text_to_speech(event.get_description())


def get_current_hours_minutes_seconds():
    """ Returns the current (hours, minutes, seconds) as a tuple. """
    t = time.localtime()
    return (t.tm_hour, t.tm_min, t.tm_sec)


def now():
    """ Returns the current time as Time object. """
    return Time(*get_current_hours_minutes_seconds())


def main():
    # print("Init:")
    # t1 = Time(9, 30, 5)
    # print("t1:", t1)
    # print("minutes", t1.get_hours())
    # print("minutes", t1.get_minutes())
    # print("seconds:", t1.get_seconds())

    # day_remaining = now()
    # day_remaining = Time(24, 00, 00) - day_remaining

    # print("Seconds remaining in current day:", day_remaining.get_total_seconds())

    # alarm_clock = AlarmClock()
    # alarm_clock.add_event( Event(Time(0, 0, 2), "event2") )
    # alarm_clock.add_event( Event(Time(0, 0, 1), "event1") )
    # alarm_clock.sort()

    # s = str(alarm_clock)
    # print(s)
    # if s.find("event1") < s.find("event2"):
    #     print("Sorted")

    alarm_clock = AlarmClock()
    alarm_clock.add_event(Event(now() + Time(0, 0, 7), "eat some breakfast"))
    alarm_clock.add_event(Event(now() + Time(0, 0, 12), "off to work"))
    alarm_clock.add_event(Event(now() + Time(0, 0, 2), "good morning, wake up"))
    alarm_clock.wait_for_and_handle_events(alarm_only_print)


# keep this at the bottom of the file
if __name__ == "__main__":
    main()

