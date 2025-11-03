      * COPYMEMBER: LIBSP/SP01GQS
      ******************************************************************
      *               (CQ)
      *    DAILY COLLECTOR GLOBAL QUEUE SUMMARY FILE
      *
      *
      * GQS1-KEY:  GQS-COLLID
      *
      *=================================================================
      *
      *  NOTE!!! THE FIELDS IN GQ2-KEY MUST MATCH THE FIELDS IN
      *          GQS-GQ2-KEY-FIELDS AREA BOTH IN SIZE & POSITION.
      *
      *=================================================================
      * REV:
      *  CS 190213 HEADER RECORD NEEDS TO BE A SEPARATE FILE
      *            SP01GQS(SUMMARY)
      *  CS 190307 KEN AND I DECIDED WE NEEDED TO HOLD THE DETAIL-GQ2-KEY
      *            WITHIN THE GQSFILE(GQS-GQ2-KEY-FIELDS). IN A15, EVERYTHING IS
      *            IN THE GQFILE, ITS DRIVEN BY TYPE THAT DETERMINES IF ITS A
      *            SUMMARY OR DETAIL GQ2 RECORD AND BOTH ARE USED TO SETUP THE
      *            GET OF THE NEXT RECORD.
      *
      *  NOTE:CS  190319 WE STILL HAVE PROBLEMS WITH DUPLICATE GQ2-KEY
      *                  ON F1-SKIP. WHATS HAPPENING IS WHEN WE F1-SKIP,
      *                  THE HEADER(GQSFILE IN A32)IS ADDING 1 TO
      *                  GQS-GQ-REV-BAL(SUMMARY FILE) FOR THE NEXT READ
      *                  OF THE DETAIL GQFILE. SO IF 2 ACCTS WITH THE SAME
      *                  GQ2-KEY,LETS SAY REV-BAL OF 998999, I SKIP THE FIRST
      *                  ACCT AND WRITE THE GQS(SUMMARY) WITH A REV-BAL OF
      *                  999000, SO ON THE NEXT READ, WE MOVE GQS-GQ-FIELDS
      *                  TO GQ2-KEY AND READ-NEXT. SINCE THE GQ2-KEY NOW HAS
      *                  A REV-BAL OF 999000, THE DUPLICATES ARE SKIPPED,
      *                  (REV-BAL 998999). IF WE DIDNT ADD 1, WE WOULD CONTINUE
      *                  TO GET THE SAME ACCOUNT.
      * JUST AS A NOTE: WE GO THRU THE SAME STEP OF ADDING 1 TO THE HEADER
      *         GQ-RECORD IN A15, AND GQ2-KEY DUPLICATES ARE BEING SKIPPED
      *         IN A15,A30,A32
      *
      *         GQ2-KEY = GQ-COLLID,GQ-REV-BEHAVIOR-RISK,GQ-SEQ-FG,
      *                   GQ-DATE,GQ-START-TIME,GQ-REV-DEL
      *                   GQ-MISC-KEY-FLD,GQ-REV-PDUE-DAYS,GQ-REV-BAL,
      *                    GQ-SEQ.
      * 190326 CS DONT THINK WE NEED GQS-GQ-BRNO, THIS IS USED TO SET UP KEY
      *           FOR NEXT READ OF GQ2 DETAIL RECORD
      * 190327 CS GQS-BRNO IS POPULATED WITH ZERO, PER KEN, REMOVE IT
      * 190603 CS REMOVE ENDING FILLER
      * 20210507 REMOVE GQS-FUTURE-KEY1,KEY2
      * BAH 20230612 ADDED FILLER
      ******************************************************************
       78  GQS-SIZE                         VALUE 120.
       01  GQS-REC.
           03  GQS1-KEY.
               05  GQS-COLLID               PIC X(3). 

      $XFD DATE=YYYYMMDD
           03  GQS-LTOUCH-DATE              PIC 9(08)   COMP-3.
           03  GQS-TOT-REC                  PIC 9(5)    COMP-3.
           03  GQS-WORKED-REC               PIC 9(5)    COMP-3.
           03  GQS-TIMED-REC                PIC 9(5)    COMP-3.
           03  GQS-GROUP-ID                 PIC X(6).
           03  GQS-GQ2-KEY-FIELDS.
               05  GQS-GQ-COLLID            PIC XXX.
               05  GQS-GQ-SEQ-FG            PIC X.
      $XFD DATE=YYYYMMDD
               05  GQS-GQ-DATE              PIC 9(8).
               05  GQS-GQ-START-TIME        PIC S9(4)   COMP-3.
               05  GQS-GQ-REV-DEL           PIC 9(5).
               05  GQS-GQ-MISC-KEY-FLD      PIC X(3).
               05  GQS-GQ-REV-PDUE-DAYS     PIC 9(5).
               05  GQS-GQ-REV-BAL           PIC 9(7).
               05  GQS-GQ-SEQ               PIC 9(3).

           03  FILLER                       PIC X(59).
