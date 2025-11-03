      * COPYMEMBER: LIBLP/LP01IC
      ****************************************************************
      *       INSURANCE COMPANY FILE     (149 BYTES)
      * REV:
      *  JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  JTG 112196 MADE IC-COMMRATE(7) PP2
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      * KEC 1999.0510 <<<<<<<< 'A60-TO-A15' CONVERSION UPDATED >>>>>>>>
      *  MJD 991001 MADE IC-COMMRATE S9(3)V99. IF NUMBER IS NEGATIVE
      *             THEN IT IS A FIXED AMOUNT, ELSE IT WILL BE HANDLED
      *             AS A PERCENTAGE (DEFAULT).
      *
      ****************************************************************
       01  IC-REC.
           03  IC1-KEY.
               05  IC-CODE             PIC 99.
           03  IC-NAME                 PIC X(20).
           03  IC-ADDR1                PIC X(25).
           03  IC-ADDR2                PIC X(25).
           03  IC-CITY                 PIC X(15).
           03  IC-STATE                PIC XX.
           03  IC-ZIP                  PIC X(10).
           03  IC-PHONE                PIC 9(10)     COMP-3.
      **************************************************************
      *    COMMISSION RATES:
      *    LEVEL: #1   CREDIT LIFE DECLINING  (SINGLE)
      *           #2     "      "      "      (JOINT)
      *           #3     "      "  WHOLE LIFE (SINGLE)
      *           #4     "      "      "      (JOINT)
      *           #5   ACCIDENT & HEALTH
      *           #6   PP
      *           #7   PP2
      *           #8   OTHER 1            (MERCURY UNEMPLOYMENT)
      *           #9   OTHER 2            (   "    UPFRONT VSI )
      *           #10  OTHER 3            (   "    WARRENTY    )
      *           #11  OTHER 4
      *           #12  OTHER 5
      *            13  N/A
      **************************************************************
           03  IC-COMMRATE             OCCURS 13 TIMES
                                       PIC S9(3)V99   COMP-3.
           03  FILLER                  PIC X(5).
