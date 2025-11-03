       LCAP-M-ADVANCE.
           PERFORM LCAP-UPD-LCPD.
           MOVE LN-REGPYAMT TO LCAP-AMTNEEDED.
           GO TO LCAP-M-BEGIN.

      *******************************************************
      * NO ASSESSMENT IS REQUIRED; ADVANCE LCAP-LCPDTH-DATE
      * TO WHERE PAYMENTS TODATE ARE PAID THRU, ONLY
      * IF IT WILL ADVANCE LCPDTH, SINCE LCPDTH COULD
      * HAVE ADVANCED BEYOND PAID THRU VIA TIME.
      *******************************************************
