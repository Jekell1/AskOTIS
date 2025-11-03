      * COPYMEMBER: LIBSP/SP01GQ
      ******************************************************************
      *               (GQ)
      *    DAILY COLLECTOR QUEUE FILE
      *
      *    GQ-TYPE:    " " = HEADER RECORD. (MN/GQHMAN.C)
      *                "D" = DETAIL RECORD. (MN/GQMAIN.C)
      *
      *     GQ1-KEY = GQ-BRNO, GQ-ACCTNO
      *     GQ2-KEY = GQ-COLLID GQ-SEQ-FG GQ-DATE GQ-START-TIME
      *               GQ-REV-DEL GQ-MISC-KEY-FLD
      *               GQ-REV-PDUE-DAYS GQ-REV-BAL  GQ-SEQ
      *
      *    NOTE - I PUT A SIGN ON GQ-TIME & ACTUALLY PUT NEGATIVE
      *    NUMBERS IN THERE FOR THE TIME ZONE FACTOR.  BUT I DON'T
      *    THINK IT MATTERS IF A COMP-3 NUMBER IS NEGATIVE...I MEAN
      *    IN AN INDEX -4 COMES BEFORE -5 (4D, 5D, ETC).  THE POSITIVE
      *    NUMBERS I THINK WOULD COME LATER SINCE THEY'D BE 4F, 5F.
      *    BUT I REALLY WANT THESE NUMBERS TO COME OUT WITH IN NUMERICAL
      *    ORDER WITH THE SIGNS ALLOWED SO THE SEQUENCE IS EAST TO WEST:
      *    EASTERN(-5), THEN  CENTRAL(-6), MOUNTAIN (-7), PACIFIC(-8)
      *
      *    ALSO, NOTE - THE FACTOR STORED HERE HAS ALREADY BEEN ADJUSTED
      *    BACK AN HOUR FOR DAYLIGHT SAVINGS.  FOR EX., CUSTOMER IS
      *    EASTERN (-5) BUT ZONE IS CODED DAYLIGHT SAVINGS & IT'S
      *    JULY SO VALUE OF GQ-TIME IS -4.
      *
      *=================================================================
      *
      * GQ-DATE & GQ-DATE-QUEUED ARE STORING IN YYYYMMDD IN A30
      * AND DOES NOT NEED TO FIXED LATER (A31)            KEC 2016.1006
      *
      *=================================================================
      * REV:
      * 040896  BLL  ADD GQ-MISC-KEY-FIELD (FOR POT PAST-DUE)
      * MJD 990125 ----- UPDATED IN FLEXGEN -----
      * BLV 000821 PUT A SIGN ON GQ-TIME - IT WAS IMPLIED,
      *            SO DIDN'T CHANGE FILE DEFINITION
      * BLV 000908 ADD GQ-TIME-ZONE-ERROR, TFC PR# 25
      * BLM 081009 ADD GQ-GROUP-ID, REGACC PR# 220
      * BLM 160913 CHANGE GQ-DATE FROM S9(7) COMP-3 TO 9(8),
      *            REMOVE COMP-3'S FROM KEY FIELDS(EXCEPT FOR
      *            GQ-START-TIME, SEE NOTES ABOVE ABOUT NEGATIVES),
      *            INCREASED GQ-REV-BAL TO 7, DECREASED GQ-PDUE-DAYS
      *            TO 4 & REMOVED REDEF, ADDED GQ-SEQ TO KEEP KEY
      *            UNIQUE, ADDED FUTURE-KEY1 & 2, INCREASE UNUSED.
      *            GQ-DATE-QUEUED WAS IN THE WEIRD 9(7), WILL JUST
      *            STORE THIS AS A NORMAL MMDDYY DATE NOW.
      *
      * KEC 2016.1006 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED GQ-LTOUCH-DATE.
      *         CHANGED GQ-REC TO 140 BYTES (GQ-SIZE).
      *  CS 190311 THE HEADER/SUMMARY RECORD IS NOW A SEPARATE FILE
      *            LIBSP/SP01GQS. REMOVED HEADER FIELDS, REMOVED FILLER
      *            IN MIDDLE OF RECORD
      *  CS 190307 KEN AND I DECIDED WE NEEDED TO HOLD THE DETAIL-GQ2-KEY
      *            WITHIN THE GQSFILE(GQS-GQ2-KEY-FIELDS). IN A15, EVERYTHING IS
      *            IN THE GQFILE, ITS DRIVEN BY TYPE THAT DETERMINES IF ITS A
      *            SUMMARY OR DETAIL GQ2 RECORD AND BOTH ARE USED TO SETUP THE
      *            GET OF THE NEXT RECORD.
      *  CS 190320 WE STILL HAVE PROBLEM WITH DUPLICATE GQ2-KEY ON F1-SKIP.
      *                  THE HEADER(GQSFILE IN A32)IS ADDING 1 TO
      *                  GQS-CQ-REV-BAL(SUMMARY FILE) FOR THE NEXT READ
      *                  OF THE DETAIL GQFILE. SO IF 2 ACCTS WITH THE SAME
      *                  GQ2-KEY,LETS SAY REV-BAL OF 998999, I SKIP THE FIRST
      *                  ACCT AND WRITE THE GQS(SUMMARY) WITH A REV-BAL OF
      *                  999000,(ADD 1_ SO ON THE NEXT READ, WE MOVE
      *                  GQS-CQ-FIELDS TO GQ2-KEY AND READ-NEXT. SINCE THE
      *                  GQ2-KEY NOW HAS A REV-BAL OF 999000, THE DUPLICATES
      *                  ARE SKIPPED,(REV-BAL 998999). IF WE DIDNT ADD 1, WE 
      *                  WOULD CONTINUE TO GET THE SAME ACCOUNT.
      * JUST AS A NOTE: WE GO THRU THE SAME STEP OF ADDING 1 TO THE HEADER
      *         GQ-RECORD IN A15, AND GQ2-KEY DUPLICATES ARE BEING SKIPPED
      *         IN A15,A30,A32
      *
      *         GQ2-KEY = GQ-COLLID,GQ-REV-BEHAVIOR-RISK,GQ-SEQ-FG,
      *                   GQ-DATE,GQ-START-TIME,GQ-FUTURE-KEY1,GQ-REV-DEL
      *                   GQ-MISC-KEY-FLD,GQ-REV-PDUE-DAYS,GQ-REV-BAL,
      *                   GQ-FUTURE-KEY2, GQ-SEQ.
      * CS 190603 ADD FUTURE FIELDS,GQ DOES NOT CONTAIN THE FIELD
      *           GQ-REV-BEHAVIOR-RISK THAT CQ DOES
      * BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      *  CS 20210506 REMOVE GQ-FUTURE-KEY1,FUTURE-KEY2, FUTURE FIELDS
      * BAH 20230612 ADDED FILLER
      ******************************************************************
       78  GQ-SIZE                      VALUE  120.
       01  GQ-REC.
           03  GQ1-KEY.
               05  GQ-BRNO              PIC 9(4).
               05  GQ-ACCTNO            PIC 9(8).
           03  GQ-COLLID            PIC XXX.
           03  GQ-SEQ-FG            PIC X.
      $XFD DATE=YYYYMMDD
           03  GQ-DATE              PIC 9(8).
           03  GQ-START-TIME        PIC S9(4)   COMP-3.
           03  GQ-SEQ-FIELDS.
      * FUTURE KEY, MAYBE POOL AND/OR ACTION?  LIKE SPECIAL ACCTS
      * THEY WANT AT BEGINNING OF DAY'S QUEUE
               05  GQ-REV-DEL       PIC 9(5).
               05  GQ-MISC-KEY-FLD  PIC X(3).
               05  GQ-REV-PDUE-DAYS PIC 9(5).
               05  GQ-REV-BAL       PIC 9(7).
               05  GQ-SEQ           PIC 9(3).

      $XFD DATE=YYYYMMDD
           03  GQ-LTOUCH-DATE       PIC 9(08) COMP-3.
           03  GQ-WORK-COLLID       PIC XXX.
           03  GQ-WORKED-FG         PIC X.
           03  GQ-IN-PROCESS        PIC X.
      $XFD DATE=YYYYMMDD
           03  GQ-DATE-QUEUED       PIC 9(8)  COMP-3.
           03  GQ-TIME-ZONE-ERROR   PIC X.

           03  FILLER               PIC X(54).
