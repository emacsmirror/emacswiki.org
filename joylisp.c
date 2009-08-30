/* Communicate between GNU Emacs and the Linux Joystick Interface.

   Copyright (C) 2007 John C. G. Sturdy

   Based on jstest.c which is Copyright (C) 1996-1999 Vojtech Pavlik (Sponsored by SuSE) and released under GPL2.

   Initially written 2007-08-29.

   This file is not part of GNU Emacs.

   The Emacs Joystick Interface is free software; you can redistribute
   it and/or modify it under the terms of the GNU General Public
   License as published by the Free Software Foundation; either
   version 2, or (at your option) any later version.

   The Emacs Joystick Interface is distributed in the hope that it
   will be useful, but WITHOUT ANY WARRANTY; without even the implied
   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
   See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with the Emacs Joystick Interface; see the file COPYING.  If
   not, write to the Free Software Foundation, Inc., 51 Franklin
   Street, Fifth Floor, Boston, MA 02110-1301, USA.

   Starting the program
   ====================

   This is normally done for you by `joystick-start' in the
   accompanying Emacs-Lisp file `joystick.el'.  In case you want to
   use this in some other application, here are the details anyway.

   Usage: joylisp [device [device-event-label [device-non-event-label]]]

   This program converts joystick events to Lisp s-expressions, and
   sends them to stdout.

   The "device" argument is the joystick device to read, such as
   /dev/js0.

   The optional "device-event-label" argument is something to put at
   the start of each event s-exp instead of "jse '" (which stands for
   JoyStick Event).  This is partly in case you have several
   joystick-like devices on the same system, and also lets you give
   something without the space and quote, so that each type of event
   runs a different Lisp function.

   The optional "device-event-label" argument is something to put at
   the start of each non-event s-exp instead of "joystick-".  This is
   partly in case you have several joystick-like devices on the same
   system.  The default is such that each type of non-event
   (declarations etc) runs a different Lisp function.

   Buttons
   =======

   When you press a button (say the button Trigger), an expression of
   the form

      (jse 'Trigger-down)

   is sent to stdout, and if that button is then released without any
   other buttons having been pressed,

      (jse 'Trigger-up)

   is output.

   If a button is pressed and held while another button is pressed,
   the "down" event for the first button is sent as before (as we
   don't have the technology to predict whether another button will be
   pressed before the first is released), but the second button, say
   button TopBtn, gets some modifiers included, either as a string of
   abbreviated button names, or as an octal number embedded in the
   functor name:

      (jse 'BaBt2-TopBtn-down)
      (jse 'BaBt2-TopBtn-up)

      (jse 'm200-TopBtn-down)
      (jse 'm200-TopBtn-up)

   where 200 is the octal code for just bit 7 (the modifier button
   number) being set.  The abbreviations are made of any upper-case
   letters in the full name of the button, any lower-case letters
   preceded by an upper-case letter, and any digits.  This seems to be
   a reasonably simple way of making unique abbreviations, giving the
   collection of button names provided.
   
   Any number of modifiers may be used at once.  (There are
   potentially nearly 80 of them, given a suitable monstrous stick!)

   When button 7 is eventually released, because it has been used as a
   modifier, instead of an "up" event, it generates a "release" event.

      (jse 'Trigger-release)

   This way, each button can be used both in its own right (press and
   release) or as a modifier (press and hold while pressing other
   buttons).

   Also, note that if you're using "-up" codes for chording, it makes
   a difference which button you take your finger off last.  This
   makes for a potentially extremely subtle chording keyboard!

   Joystick axes
   =============

   When you move a "stick" control, there are three modes of output
   written into the code, called "timing", "up_down" and "signed".  At
   the moment, it is hardwired to use "timing", but when commands to
   this program from the calling program (e.g. Emacs) are implemented,
   it'll be switchable.

   In timing mode, each joystick event is sent as one of these:

     (jse 'X-previous)
     (jse 'X-center)
     (jse 'X-next)

   and the event is sent repeatedly (except for -center), at a rate
   determined by the displacement of that stick axis from its centered
   position.

   In up_down mode, each joystick event is sent as:

     (jse 'X-previous amount)
     (jse 'X-center)
     (jse 'X-next amount)

   according to whether the direction is the one Emacs regards as
   "previous" (i.e. up or left, depending on the axis) or as "next"
   (down or right), or "center" which is output when the joystick
   returns to the center.  The "amount" is the number received from
   the joystick event, and is the amount (unsigned) by which the stick
   is displaced from the center.  The amount for "center" is
   implicitly 0, so is not sent.

   In signed mode, the joystick events are sent as:

     (jse 'X amount)

   where the amount may be positive or negative.

   In signed and up_down modes, the events are not automatically
   repeated.

   In any case, modifier numbers are sent in octal:

     (jse 'm200-X-previous amount)
     (jse 'm200-X-center)
     (jse 'm200-X-next amount)

   and it is remembered that those buttons have been used as
   modifiers, and hence generate "release" instead of "up" when
   released.

   Controlling the interface program
   =================================

   The program accepts "shell-style" commands on stdin.  The commands
   are as follows:

     acknowledge [arg]

       With arg == 0, turn command acknowledgment off; otherwise turn
       it on.

     hatspeed
     hatspeed value
     hatspeed channel
     hatspeed channel value

       Set the hat speed.  This is the same as `sensitivity', but only
       applies to axes that are described as hat switch axes.  (These
       are `all-or-nothing' joystick axes, so you may well want to
       make them less sensitive than the ordinary ones.)

     numeric-mods
     symbolic-mods

       Set the modifier representation to numeric or symbolic.

     quit

       Quit the joystick-to-lisp program.

     rumble

       Make the joystick / keypad rumble.  Not implemented, as there's
       no mention of this in the joystick driver documentation.

     sensitivity
     sensitivity value
     sensitivity axis value
     sensitivity axis
     
       With no args, or with only an axis name, report all or one of
       the sensitivities.

       With a value, and with or without an axis name, set one or all
       of the sensitivities.

       Value should be a floating-point value between 0.0 and 1.0; it
       is the proportion of internal ticks for which full displacement
       of the joystick in this output will produce an event in
       `timing' mode.  So, for example, if you set an axis'
       sensitivity to 0.25, it will produce an event at most once
       every 4 ticks.

     shock

       Make the joystick / keypad give the user a shock.  Not
       implemented, as there's no mention of this in the joystick
       driver documentation.  Also, this is just a joke entry from a
       James Bond film, but I wouldn't be surprised if someone really
       makes one sometime.

     show-ticking [arg]

       With arg == 0, don't show every internal tick.
       Otherwise, show all the internal ticks.
       Mostly meant for debugging.

     signed

       Output the joystick displacement as a signed value.  See also
       `timing' and `updown'.

     stamped [arg]

       With arg == 0, don't send timestamps before each event.
       Otherwise, send a timestamp before each event.

     tickrate [ticks-per-second]

       With argument, set the ticks per second (for the `timing'
       output) to ticks-per-second.

       With no argument, report the current tick rate.

       If you set this to a larger value (and you'll probably want to
       turn `sensitivity' down in that case), you get finer
       discrimination of the joystick position, at the expense of
       using more CPU time.  Still, that probably won't be much.

     timing

       When a joystick axis is off-center, send a stream of events, at
       a rate determined by how far off-center it is.  This is the
       default.  See also `signed' and `updown'.  The scale of the
       individual axis is set with the `sensitivity' command, and the
       overall rate of the system is set with `tickrate'.

     updown

       Output the joystick displacement as an unsigned value, and give
       the sign by issuing different event types.  See also `signed'
       and `timing' output modes.

 */


#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/types.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <ctype.h>
#include <math.h>

#include <linux/input.h>
#include <linux/joystick.h>

char *axis_names[ABS_MAX + 1] = {
  /* most of these come straight from jstest.c, but I finished
     numbering them, to make them all unique */
  "X", "Y", "Z",		/* 0,1,2 */
  "Rx", "Ry", "Rz",		/* 3,4,5 */
  "Throttle", "Rudder",		/* 6,7 */
  "Wheel", "Gas", "Brake",	/* 8,9,10 */
  "Ax11", "Ax12", "Ax13", "Ax14", "Ax15", /* 11,12,13,14,15 */
  "Hat0X", "Hat0Y",		/* 16,17 */
  "Hat1X", "Hat1Y",		/* 18,19 */
  "Hat2X", "Hat2Y",		/* 20,21 */
  "Hat3X", "Hat3Y",		/* 22,23 */
  "Ax24", "Ax25", "Ax26", "Ax27", /* 24,25,26,27 */
  "Ax28", "Ax29", "Ax30",	  /* 28,29,30 */
};

/* Hat joysticks give "full displacement" or "no displacement" and
   nothing inbetween.  This means they repeat very fast, unless we take
   special measures.  So, we note the axis numbers that belong to
   hats. */
#define HAT_MIN 16
#define HAT_MAX 23

#define Axis_Name(_a_) (axis_names[axmap[(_a_)]])

char *button_names[KEY_MAX - BTN_MISC + 1] = {
  /* most of these come straight from jstest.c, but I finished
     numbering them, to make them all unique */
  "Btn0", "Btn1", "Btn2", "Btn3", "Btn4", 
  "Btn5", "Btn6", "Btn7", "Btn8", "Btn9", /* 0-9 */
  "Btn10", "Btn11", "Btn12", "Btn13", "Btn14", "Btn15",	   /* 10-15 */
  "LeftBtn", "RightBtn", "MiddleBtn", "SideBtn",	   /* 16-19 */
  "ExtraBtn",						   /* 20 */
  "ForwardBtn", "BackBtn",				   /* 21,22 */
  "TaskBtn",						   /* 23 */
  "Btn24", "Btn25", "Btn26", "Btn27", "Btn28", "Btn29", "Btn30", "Btn31", /* 24-31 */
  "Trigger", "ThumbBtn", "ThumbBtn2", "TopBtn", "TopBtn2", /* 32-36 */
  "PinkieBtn",						   /* 37 */
  "BaseBtn", "BaseBtn2", "BaseBtn3", "BaseBtn4", "BaseBtn5", "BaseBtn6", /* 38-43 */
  "BtnDead",			/* 44 */
  "BtnA", "BtnB", "BtnC",	/* 45-47 */
  "BtnX", "BtnY", "BtnZ",	/* 48-50 */
  "BtnTL", "BtnTR", 		/* 51,52 */
  "BtnTL2", "BtnTR2",		/* 53,54 */
  "BtnSelect", "BtnStart",	/* 55,56 */
  "BtnMode",			/* 57 */
  "BtnThumbL", "BtnThumbR",	/* 58,59 */
  "Btn60", "Btn61", "Btn62", "Btn63", 
  "Btn64", "Btn65", "Btn66", "Btn67",
  "Btn68", "Btn69", "Btn70", "Btn71", 
  "Btn72", "Btn73", "Btn74", "Btn75", "Btn76", /* 60-76 */
  "WheelBtn",			/* 77 */
  "Gear up",			/* 78 */
};

#define Button_Name(_b_) (button_names[btnmap[(_b_)] - BTN_MISC])

#define NAME_LENGTH 128

/* This should be large enough, as the only variable parts output for
   joystick events are numbers and modifiers.  The maximum modifiers
   on a maximum monstrous joystick (that would be about 80 buttons,
   all pressed at the same time) come to about 500 chars. */
#define OUTPUT_BUF_SIZE 1024
#define COMMAND_BUF_SIZE 1024

/* max stick displacement; dividing this by the actual displacement
   gives 1, i.e. send a simulated event every 1 tick, when the stick
   is displaced maximally */

#define STICK_MAX_DISPLACEMENT 32767.0

static char output_buf[OUTPUT_BUF_SIZE];

static char command_buf[COMMAND_BUF_SIZE];


/* The first thing inside the brackets for all the event s-exps we issue.
   The user can set this (as the second arg when calling the program);
   the default value is a Lisp symbol followed by a space, and quote
   for what follows; but you could make it the start of a symbol name,
   without the space, so that each event type calls a different Lisp
   function. */
static char *joystick_event_name = "jse '";

/* The first thing inside the brackets for all the NON-event s-exps we
   issue.  The user can set this (as the third arg when calling the
   program); the default value is "joystick", so that each event type calls a
   different Lisp function. */
static char *joystick_name = "joystick-";

/* whether we are sending timestamps */
static int timestamped = 0;

/* Whether we are doing the timing, or leaving that to our consumer process */
static int timing = 1;

/* Whether we are reporting axis up and down as separate commands.
   static.  This has effect only if timing == 0. */
static int up_down = 1;

/* Whether we are still running: */
static int running = 1;

/* Used to set the repeat rate for "all-or-nothing" joystick axes. */
static int hat_sensitivity = 1.0;

/* Whether we are acknowledging commands */
static int acknowledge = 0;

/* Whether we are showing ticks -- mostly for debugging */
static int show_ticking = 0;


void
output(char *fmt, ...)
{
  va_list ap;
  int formatted;
  va_start(ap, fmt);

  formatted = vsnprintf(output_buf, OUTPUT_BUF_SIZE, fmt, ap);

  va_end(ap);

  write(1, output_buf, strlen(output_buf));

  output_buf[0] = '\0';
}

unsigned int
getstamptime()
/* return milliseconds since midnight */
{
  struct timeval tv;
  if (gettimeofday(&tv, NULL) == 0)
    {
      return (tv.tv_usec / 1000) + ((tv.tv_sec % (24 * 60 * 60)) * 1000);
    } else {
    return 0;
  }
}


static unsigned long modifiers = 0;
/* If a button is used as a modifier, when it goes up we say "release"
   rather than "up", so each button can be used as either a modifier
   (if any other buttons are pressed before it is released) or a
   button in its own right (if no other buttons are pressed before it
   is released).  So, we need to remember which buttons have been
   actually used as modifiers. */
static unsigned long used_modifiers = 0;

/* 5*80 should be enough for all possible modifiers pressed at once */
static char modifiers_buf[512];
/* abbreviations of button names */
static char modifier_names[512];

static int symbolic_modifiers = 1;

static char *btn_abbrevs[KEY_MAX - BTN_MISC + 1];

void
set_modifiers_buffer()
/* fill in modifiers_buf according to modifiers */
{
  if (modifiers) {
    if (symbolic_modifiers) {
      unsigned int i = 0;
      unsigned int mods = modifiers;
      char *dest = modifiers_buf;
      while (mods) {
	if (mods & 1) {
	  char *src = btn_abbrevs[i];
	  while (*src) {
	    *dest++ = *src++;
	  }
	  *dest++ = '-';
	}
	i++;
	mods = mods >> 1;
      }
      *dest = '\0';
    } else {
      sprintf(modifiers_buf, "m%o-", modifiers);
    }
  } else {
    modifiers_buf[0] = '\0';
  }
}

int
main (int argc, char **argv)
{
  int fd;

  struct js_event js;
  struct timeval tv;
  fd_set set;

  unsigned char naxes = 2;
  unsigned char nbuttons = 2;
  int version = 0x000800;
  char name[NAME_LENGTH] = "Unknown";
  uint16_t btnmap[KEY_MAX - BTN_MISC + 1];
  uint8_t axmap[ABS_MAX + 1];

  unsigned int *rates;
  int *countdowns;
  double *sensitivities;
  char **actions;

  /* Commands don't always come in in a single read; this is for
     accumulating them until we get EOL: */
  char *command_reading = command_buf;

  int i;

  int tick_secs = 0;
  int tick_usecs = 250000;

  char *joystick_device = "/dev/js0";

  set_modifiers_buffer();

  if ((argc >= 2) && (argv[1][0] != '\0')) {
    joystick_device = argv[1];
  }

  if ((fd = open(joystick_device, O_RDONLY)) < 0) {
    perror("joylisp");
    return 1;
  }

  /* Allow the caller to name the joystick, in case there are several
     on the system.  Rather than fiddling round to vary the number of
     args in the call to call-process, I pass the empty string if the
     user doesn't want to label it, so look out for that. */
  if ((argc >= 3) && (argv[2][0] != '\0')) {
    joystick_event_name = argv[2];
    if (strlen(joystick_event_name) > 512) {
      /* avoid buffer overruns from over-long label names; truncate
	 quietly, they deserve nothing better ;-) */
      joystick_event_name[511] = '\0';
    }
  }

  if ((argc >= 4) && (argv[3][0] != '\0')) {
    joystick_event_name = argv[3];
    if (strlen(joystick_name) > 512) {
      /* avoid buffer overruns from over-long label names; truncate
	 quietly, they deserve nothing better ;-) */
      joystick_name[511] = '\0';
    }
  }

  ioctl(fd, JSIOCGVERSION, &version);
  ioctl(fd, JSIOCGAXES, &naxes);
  ioctl(fd, JSIOCGBUTTONS, &nbuttons);
  ioctl(fd, JSIOCGNAME(NAME_LENGTH), name);
  ioctl(fd, JSIOCGAXMAP, axmap);
  ioctl(fd, JSIOCGBTNMAP, btnmap);

  output("(%sdeclare-version \"%d.%d.%d\")\n",
	 joystick_name,
	 /* I'm only guessing, I couldn't find the description of
	    this in the header files */
	 version >> 16,
	 (version & 0xffff) >> 8,
	 version & 0xff);
  output("(%sdeclare-buttons %d)\n",
	 joystick_name, nbuttons);
  output("(%sdeclare-axes %d)\n", 
	 joystick_name, naxes);
  output("(%sdeclare-name \"%s\")\n",
	 joystick_name, name);

  rates = (unsigned int*) malloc ((naxes + 1) * sizeof(unsigned int));
  countdowns = (int*) malloc ((naxes + 1) * sizeof(int));
  sensitivities = (double*) malloc ((naxes + 1) * sizeof(double));
  actions = (char**) malloc ((naxes + 1) * sizeof(char*));

  for (i = 0; i < naxes; i++) {
    countdowns[i] = rates[i] = 0;
    sensitivities[i] = 1.0;
    actions[i] = "-idle";
  }

  for (i = HAT_MIN; i <= HAT_MAX; i++) {
    sensitivities[i] = hat_sensitivity;
  }

  /* make abbreviations of button names */
  {
    char *next = modifier_names;
    for (i = 0; i < nbuttons; i++) {
      char prev;
      char *button_name = Button_Name(i);
      btn_abbrevs[i] = next;
      prev = *button_name;
      for (; *button_name != '\0'; button_name++) {
	if (isupper(*button_name) || isupper(prev) || isdigit(*button_name)) {
	  *next++ = *button_name;
	}
	prev = *button_name;
      }
      *next++ = '\0';
    }
  }

  /* Make stdin non-blocking */
  {
    int flags = fcntl(0, F_GETFL);
    fcntl(0, F_SETFL, flags | O_NONBLOCK /* | O_DIRECT */);
  }

  while (running) {

    FD_ZERO(&set);
    FD_SET(fd, &set);
    FD_SET(0, &set);

    int command_length;
    int selected;

    tv.tv_sec = tick_secs;
    tv.tv_usec = tick_usecs;

    selected = select(fd+1, &set, NULL, NULL, &tv);

    switch (selected) {
    case -1:
      perror("joylisp");
      exit(1);
      break;		/* for lintage */

    case 0:
      /* In the case of a timeout on the select, we step all the
	 things that could be doing countdowns, and output events for
	 any that have reached 0. */
      if (show_ticking) {
	output("(%stick)\n", joystick_event_name);
      }
      for (i = 0; i < naxes; i++) {
	if (rates[i]) {
	  if (--countdowns[i] == 0) {
	    countdowns[i] = rates[i];
	    if (timestamped) {
	      unsigned int stamptime = getstamptime();
	      output("(%stimestamp %d)\n", joystick_event_name, stamptime);
	    }
	    output("(%s%s%s%s)\n",
		   joystick_event_name,
		   modifiers_buf,
		   Axis_Name(i),
		   actions[i]);
	    used_modifiers |= modifiers;
	  }
	}
      }
      break;

    default:
      if ((command_length = read(0, command_reading, COMMAND_BUF_SIZE)) > 0)
	{
	  char command_name[COMMAND_BUF_SIZE];
	  double float_arg;
	  int numeric_arg;
	  int has_numeric_arg = 0;
	  char name_arg[COMMAND_BUF_SIZE];
	  int has_name_arg = 0;
	  int channel = -1;	/* index into all the possible buttons/axes */
	  int channel_index = -1; /* index into the buttons/axes that we actually have */
	  int channel_type = -1;
	  char *command_end;

	  command_reading[command_length] = '\0';
	  command_reading += command_length;

	  if ((command_end = strchr(command_buf, '\n')) != 0) {
	    
	    int cmd_n_parts = sscanf(command_buf, "%s %lf", command_name, &float_arg);

	    *command_end = '\0'; /* for neat acknowledgement */

	    if (cmd_n_parts == 2) {
	      /* command has numeric arg only */
	      numeric_arg = (int)float_arg;
	      has_numeric_arg = 1;
	    } else if ((cmd_n_parts = sscanf(command_buf,
					     "%s %s %lf",
					     command_name,
					     name_arg,
					     &float_arg)) >= 2) {
	      if (cmd_n_parts == 3) {
		numeric_arg = (int)float_arg;
		has_numeric_arg = 1;
	      }
	      /* command has channel (and perhaps numeric arg) */
	      /* look for the channel number and type */
	      for (i = 0; i < nbuttons; i++) {
		if ((Button_Name(i) != NULL) &&
		    (strcmp(name_arg, Button_Name(i)) == 0)) {
		  channel = btnmap[i];
		  channel_type = JS_EVENT_BUTTON;
		  channel_index = i;
		  has_name_arg = 1;
		  break;
		}
	      }

	      if (channel == -1) {
		for (i = 0; i < naxes; i++) {
		  if ((Axis_Name(i) != NULL) &&
		      (strcmp(name_arg, Axis_Name(i)) == 0)) {
		    channel = axmap[i];
		    channel_type = JS_EVENT_AXIS;
		    channel_index = i;
		    has_name_arg = 1;
		    break;
		  }
		}
	      }
	    } else {
	      /* command name only */
	    }

	    if (acknowledge) {
	      output("(command-acknowledge \"%.64s\")\n", command_buf);
	    }

	    if (strcmp(command_name, "quit") == 0) {
	      running = 0;
	    } else if (strcmp(command_name, "rumble") == 0) {
	      /* send a rumble command to game controllers that support it */
	      /* unfortunately, the Linux Joystick Driver doesn't
		 support it (yet); the person to ask would be Vojtech
		 Pavlik <vojtech@ucw.cz> */
	    } else if (strcmp(command_name, "shock") == 0) {
	      /* give the user an electric shock, like in that James
		 Bond film ;-) --- does that require an opto-isolated
		 joystick to avoid damaging the computing circuitry? PS
		 only joking, I don't know of any real joysticks that do
		 this -- alias it to rumble */
	    } else if (strcmp(command_name, "updown") == 0) {
	      /* send different events for the two directions of each
		 axis, with an unsigned number for the displacement from
		 center */
	      timing = 0;
	      up_down = 1;
	    } else if (strcmp(command_name, "signed") == 0) {
	      /* send the same event for both directions of the same
		 axis, with a signed number for the displacement from
		 center */
	      timing = 0;
	      up_down = 0;
	    } else if (strcmp(command_name, "timing") == 0) {
	      /* send different events for the two directions of each
		 axis, without any numbers, but repeating at an interval
		 set from the displacement from center */
	      timing = 1;
	    } else if (strcmp(command_name, "show-ticking") == 0) {
	      if (has_numeric_arg) {
		show_ticking = numeric_arg;
	      } else {
		show_ticking = 1;
	      }
	    } else if (strcmp(command_name, "symbolic-mods") == 0) {
	      /* send modifiers using abbreviated names */
	      symbolic_modifiers = 1;
	    } else if (strcmp(command_name, "numeric-mods") == 0) {
	      /* send combined modifiers as octal number */
	      symbolic_modifiers = 0;
	    } else if (strcmp(command_name, "stamped") == 0) {
	      /* send a timestamp before each action */
	      if (cmd_n_parts == 2) {
		timestamped = numeric_arg;
	      } else {
		timestamped = 1;
	      }
	    } else if (strcmp(command_name, "tickrate") == 0) {
	      /* set the tick rate */
	      double tick_time = 1.0 / float_arg;
	      if (has_numeric_arg) {
		tick_secs = (int)(floor(tick_time));
		tick_usecs = (int)(((tick_time - (double)tick_secs)) * 1000000.0);
	      } else {
		output("(joystick-current-tick-rate %lf)\n", 1.0 / (((double)tick_secs) + (((double)tick_usecs) / 1000000.0)));
	      }
	    } else if (strcmp(command_name, "sensitivity") == 0) {
	      /* set or show the sensitivity, either overall or specifically */
	      if (has_name_arg) {
		if (has_numeric_arg) {
		  if ((channel_type == JS_EVENT_AXIS) &&
		      (channel_index >= 0) &&
		      (channel_index < naxes)) {
		    sensitivities[channel_index] = float_arg;
		  }	
		} else {
		  output("(axis-sensitivity \"%s\" %lf)\n", Axis_Name(channel_index), sensitivities[channel_index]);
		}
	      } else {
		if (has_numeric_arg) {
		  for (i = 0; i < naxes; i++) {
		    if ((i < HAT_MIN) || (i > HAT_MAX)) {
		      sensitivities[i] = float_arg;
		    }
		  }
		} else {
		  for (i = 0; i < naxes; i++) {
		    output("(axis-sensitivity \"%s\" %lf)\n", Axis_Name(i), sensitivities[i]);
		  }		}
	      }
	    } else if (strcmp(command_name, "hatspeed") == 0) {
	      /* set or show the hat speed, either overall or specifically */
	      if (has_name_arg) {
		if (has_numeric_arg) {
		  if ((channel_type == JS_EVENT_AXIS) &&
		      (channel_index >= 0) &&
		      (channel_index < naxes) &&
		      (channel >= HAT_MIN) &&
		      (channel <= HAT_MAX))
		
		    sensitivities[channel_index] = float_arg;
		} else {
		  output("(hatspeed \"%s\" %lf)\n", Axis_Name(channel_index), sensitivities[channel_index]);
		}
	      } else {
		if (has_numeric_arg) {
		  for (i = 0; i <= naxes; i++) {
		    int j = axmap[i];

		    if ((j >= HAT_MIN) && (j <= HAT_MAX)) {
		      sensitivities[i] = float_arg;
		    }
		  }
		} else {
		  for (i = 0; i <= naxes; i++) {
		    int j = axmap[i];

		    if ((j >= HAT_MIN) && (j <= HAT_MAX)) {
		      output("(hatspeed \"%s\" %lf)\n",
			     Axis_Name(i),
			     sensitivities[i]);
		    }
		  }
		}
	      }
	    } else if (strcmp(command_name, "acknowledge") == 0) {
	      if (cmd_n_parts == 2) {
		acknowledge = numeric_arg;
	      } else {
		acknowledge = 1;
	      }
	    } else {
	      output("(%sbad-command \"%s\")", joystick_event_name, command_buf);
	    }
	    command_reading = command_buf;
	  }
	  continue;
	}

      if (read(fd, &js, sizeof(struct js_event)) != sizeof(struct js_event)) {
#if 0
	output("(%sread-error)\n", joystick_event_name);
#endif
	continue;
      }

      if (timestamped) {
	unsigned int stamptime = getstamptime();
	output("(%stimestamp %d)\n", joystick_event_name, stamptime);
      }

      switch (js.type) {
      case JS_EVENT_BUTTON:
	if (js.value) {

	  /* value != 0: button has been pressed */
	  output("(%s%s%s-down)\n",
		 joystick_event_name,
		 modifiers_buf,
		 Button_Name(js.number));

	  /* all the current modifiers have now been used */
	  used_modifiers |= modifiers;

	  /* add to modifiers after output, so it doesn't modify itself */
	  modifiers |= (1 << js.number);
	  set_modifiers_buffer();

	} else {

	  /* value == 0: button has been released */

	  char *action = ((1 << js.number) & used_modifiers) ? "release" : "up";

	  /* take it out of used_modifiers, as it's no longer an active modifier */
	  used_modifiers &= ~(1 << js.number);

	  /* remove from modifiers before output, so it doesn't modify itself */
	  modifiers &= ~(1 << js.number);
	  set_modifiers_buffer();

	  output("(%s%s%s-%s)\n",
		 joystick_event_name,
		 modifiers_buf,
		 Button_Name(js.number),
		 action);
	}
	break;
      case JS_EVENT_AXIS:
	{
	  char *action;
	  int has_value = 1;
	  int value = js.value;
	  unsigned int which_axis = js.number;

	  if (up_down) {
	    if (value == 0) {
	      action = "-center";
	      has_value = 0;
	    } else if (value < 0) {
	      action = "-previous";
	      value = -value;
	    } else {
	      action = "-next";
	    }
	  } else {
	    action = "";
	  }

	  if (timing) {
	    double proportion;
	    if (value < 0) {
	      value = -value;
	    }
	    proportion = ((double)value) / STICK_MAX_DISPLACEMENT;
	    /* If using timing, don't issue an event immediately, but
	       wait for the countdown mechanism to do it, unless it is
	       starting from centered or has just gone back to
	       centered.  Otherwise, we get an extra event every time
	       the value changes, which can be very often, and makes
	       the joystick speed up whenever you move it. */
	    if ((value == 0) || (rates[which_axis] == 0)) {
	      output("(%s%s%s%s)\n",
		     joystick_event_name,
		     modifiers_buf,
		     Axis_Name(which_axis),
		     action);
	    }

	    countdowns[which_axis] =
	      rates[which_axis] =
	      (value == 0) ? 0 : sensitivities[which_axis] / proportion;
	    actions[which_axis] = action;
	  } else {
	    if (has_value) {
	      output("(%s%s%s%s %d)\n",
		     joystick_event_name,
		     modifiers_buf,
		     Axis_Name(which_axis),
		     action,
		     value);
	    } else {
	      output("(%s%s%s%s)\n",
		     joystick_event_name,
		     modifiers_buf,
		     Axis_Name(which_axis),
		     action);
	    }
	  }
	  used_modifiers |= modifiers;
	}
	break;
      case JS_EVENT_INIT | JS_EVENT_BUTTON:
	output("(%sdeclare-button %d '%s)\n",
	       joystick_name,
	       js.number,
	       Button_Name(js.number));
	break;
      case JS_EVENT_INIT | JS_EVENT_AXIS:
	output("(%sdeclare-axis %d '%s)\n",
	       joystick_name,
	       js.number,
	       Axis_Name(js.number));
	break;
      default:
	if (modifiers) {
	  output("(%sevent-%d%s %d %d)\n",
		 joystick_event_name,
		 js.type,
		 js.number,
		 modifiers_buf,
		 js.value);
	  used_modifiers |= modifiers;
	} else {
	  output("(%sevent-%d %d %d)\n",
		 joystick_event_name,
		 js.type,
		 js.number,
		 js.value);
	}
	break;
      }
    }
  }

  exit(0);
}
