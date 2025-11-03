      * COPYMEMBER: LIBLP/LP01IC
      ****************************************************************
      *
      * DESCRIPTION:  INSURANCE COMPANY FILE (250 BYTES)
      *
      *=================================================================
      * REV:
      * JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      * JTG 112196 MADE IC-COMMRATE(7) PP2
      * MJD 990126 ----- UPDATED IN FLEXGEN -----
      * KEC 1999.0510 <<<<<<<< 'A60-TO-A15' CONVERSION UPDATED >>>>>>>>
      * MJD 991001 MADE IC-COMMRATE S9(3)V99. IF NUMBER IS NEGATIVE
      *            THEN IT IS A FIXED AMOUNT, ELSE IT WILL BE HANDLED
      *            AS A PERCENTAGE (DEFAULT).
      *  CS 160114 ADD 1 LEVEL OF COMMISSIONS, OCCURS 11,12,13 NOT USED
      *            NEED 4 NEW TOTAL
      *  CS 160114 ADDED IC-BRNO
      *  CS 160204 ADDED IC-LTOUCH-DATE 
      ****************************************************************
       78  IC-SIZE                     VALUE 250.
       01  IC-REC.
           03  IC-BRNO                 PIC 9(4).
           03  IC1-KEY.
               05  IC-CODE             PIC 99.
      *>>XDD DATE=YYYYMMDD
           03  IC-LTOUCH-DATE          PIC 9(8) COMP-3.
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
      *    IN A15, 11,12,AND #13 NOT USED
      *           #11  OTHER 4
      *           #12  OTHER 5
      *           #13  N/A NOW OTHER 6
      *           #14  NEW OTHER 7
      **************************************************************
           03  IC-COMMRATE             OCCURS 14 TIMES
                                       PIC S9(3)V99   COMP-3.
           03  FILLER                  PIC X(94).
