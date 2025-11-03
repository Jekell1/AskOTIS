      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMEMO.
       DATE-WRITTEN.    092404.
      *******************************************************
      *  DESC:  LOAN PROCESSING
      *  CALLED FROM:  ALL SPMAN* PROGRAMS
      *
      *         WRITE A MEMO RECORD FOR THE CHANGE MADE TO
      *         THE SPR RECORD
      *
      * REV:
      *  040924 NEW FOR WORLD PR# 362
      *  120618 BAH ADDED NEW FIELDS SINCE PROGRAM WAS WRITTEN
      *  KEC 2015.1111 CHANGED SM-SEQ FROM 2 TO 3, ADDED SM-REAL-DATE
      *  KEC 2015.1204 INCREASE SM-SP-SPRCLASS TO BE 3 BYTES.
      *  CS 160304 ADD SP-ALT-PREPAY-MONTHS
      *  KEC 2017.0306 REVISED THE 'REPLACING LEADING' ON SP-REC TO INCLUDE
      *                THE INDEXES USED IN OCCURS CLAUSES.
      *  CS 171114 FIXED HOLD-SM-DATE. DID GETDATE(8), MOVED TO MMDDYY
      *            CONVERTED TO 8.  GETDATE CONVERTS ALPDATE TO 9(8) AND
      *            MOVES TO SYS-DATE
      *  171212 BAH ADDED SP-NON-LOAN-FLAG AND SP-SKIP-TRW WORLD# 1288A
      *  181016 BAH PP, PP2, OT1 INSURANCES HAD WRONG PAGE NUMBER, #1342
      *  181017 BAH CHGABLE FIELD 07 WAS LOGGING AS FIELD 31, #1342
      *  190213 BAH REMOVED REVOLVING. HARD CODED OR-REC AND CH-REC
      *             REPLACING OF A REPLACING WAS NOT WORKING
      *  20210719 BAH "AH INS RETRO" WAS LOGGING AS P14F14, SHOULD BE
      *               P09F14, ALSO FIXED "DAYS WAIT" #1507
      *******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./CL/SPMEMO.CBL A30 11/14/17-14:09:13 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01SM.CPY".
       COPY "LIBLP/LP01SM_SQL.CPY".
       COPY "LIBLP/LPWSSM.CPY".

       01  ERRCD               PIC X VALUE " ".
       01  SUB                 PIC 9(2) VALUE ZEROS.
       01  S1                  PIC 9(2) VALUE ZEROS.
       01  FIELD-NO            PIC 9(2) VALUE ZEROS.

       01  MEMO-AREA.
           03  MEMO-FIELD            PIC X(12).
           03  FILLER REDEFINES MEMO-FIELD.
               05  MEMO-INS-TY       PIC X(4).
               05  REST-OF-MEMO-INS  PIC X(08).
           03  FILLER  REDEFINES MEMO-FIELD.
               05  MEMO-REB-TY       PIC X(5).
               05  REST-OF-MEMO-REB  PIC X(7).
           03  FILLER  REDEFINES MEMO-FIELD.
               05  MEMO-EARN-TY       PIC X(4).
               05  REST-OF-MEMO-EARN  PIC X(8).

           03  FILLER                PIC X VALUE ":".
           03  MEMO-PG-FLD           PIC X(5) VALUE SPACES.
           03  MEMO-P-F REDEFINES MEMO-PG-FLD.
               05  MEMO-PG           PIC 9(2).
               05  MEMO-SEP          PIC X.
               05  MEMO-FLD          PIC X(2).
               05  MEMO-FLD-NUM REDEFINES MEMO-FLD PIC 9(2).
           03  FILLER                PIC X VALUE SPACES.
           03  MEMO-OR-CHAR          PIC X(10).
           03  FILLER REDEFINES MEMO-OR-CHAR.
               05  MEMO-OR-DATE      PIC 9999/99/99.
           03  FILLER REDEFINES MEMO-OR-CHAR.
               05  FILLER            PIC X(2).
               05  MEMO-OR-NUM       PIC ZZZZZZZ9.
           03  FILLER REDEFINES MEMO-OR-CHAR.
               05  MEMO-OR-DEC       PIC ZZZ,ZZ9.99.
           03  FILLER REDEFINES MEMO-OR-CHAR.
               05  FILLER            PIC X(3).
               05  MEMO-OR-DEC3      PIC ZZ9.999.
           03  FILLER REDEFINES MEMO-OR-CHAR.
               05  FILLER            PIC X(2).
               05  MEMO-OR-DEC4      PIC ZZ9.9999.
           03  FILLER REDEFINES MEMO-OR-CHAR.
               05  FILLER            PIC X(5).
               05  MEMO-OR-DEC1      PIC ZZ9.9.
           03  FILLER                PIC X     VALUE " ".
           03  FILLER                PIC X(3)  VALUE "TO:".
           03  MEMO-CH-CHAR          PIC X(10).
           03  FILLER REDEFINES MEMO-CH-CHAR.
               05  MEMO-CH-DATE      PIC 9999/99/99.
           03  FILLER REDEFINES MEMO-CH-CHAR.
               05  FILLER            PIC X(2).
               05  MEMO-CH-NUM       PIC ZZZZZZZ9.
           03  FILLER REDEFINES MEMO-CH-CHAR.
               05  MEMO-CH-DEC       PIC ZZZ,ZZ9.99.
           03  FILLER REDEFINES MEMO-CH-CHAR.
               05  FILLER            PIC X(3).
               05  MEMO-CH-DEC3      PIC ZZZ.999.
           03  FILLER REDEFINES MEMO-CH-CHAR.
               05  FILLER            PIC X(2).
               05  MEMO-CH-DEC4      PIC ZZZ.9999.
           03  FILLER REDEFINES MEMO-CH-CHAR.
               05  FILLER            PIC X(5).
               05  MEMO-CH-DEC1      PIC ZZ9.9.
           03  FILLER                PIC X.
           03  MEMO-USERID           PIC X(8).
           03  FILLER                PIC X.
           03  MEMO-TIME             PIC X(7).

       01  HOLD-SM1-KEY.
           03  HOLD-SM-SP1-KEY.
               05  HOLD-SM-SP-ORGST       PIC X(2).
               05  HOLD-SM-SP-SPRCLASS    PIC 9(3).
               05  HOLD-SM-SP-SUBCLASS    PIC 9(2).
               05  HOLD-SM-SP-LAWCODE     PIC 9(2).
           03  HOLD-SM-DATE               PIC 9(8).
           03  HOLD-SM-SEQ                PIC 9(3).

      *****************************************************
      * ORIGINAL REC
      *****************************************************
       78  SP-SIZE                            VALUE 5146.
       01  OR-REC.
           03  OR1-KEY.
               05  OR-ORGST                   PIC XX.
               05  OR-SPRCLASS                PIC 999.
               05  OR-SUBCLASS                PIC 99.
               05  OR-LAWCODE                 PIC 99.
       COPY "LIBLP/LP01SPA_AREA.CPY" REPLACING LEADING "SPA-" BY "OR-"
                                     LEADING "AEXPR" BY "OEXPR"
                                     LEADING "AASTP" BY "OASTP"
                                     LEADING "ACTBL" BY "OCTBL"
                                     LEADING "ACSTP" BY "OCSTP"
                                     LEADING "AETBL" BY "OETBL"
                                     LEADING "AESTP" BY "OESTP".
      * SIZE=2733
       COPY "LIBLP/LP01SPB_AREA.CPY" REPLACING LEADING "SPB-" BY "OR-"
                                        LEADING "BILEV" BY "OILEV"
                                        LEADING "BISTP" BY "OISTP"
                                        LEADING "BIVER" BY "OIVER".
      * SIZE=2231
       COPY "LIBLP/LP01SPC_AREA.CPY" REPLACING LEADING "SPC-" BY "OR-".



      *****************************************************
      * CHANGED REC
      *****************************************************
       01  CH-REC.
           03  CH1-KEY.
               05  CH-ORGST                   PIC XX.
               05  CH-SPRCLASS                PIC 999.
               05  CH-SUBCLASS                PIC 99.
               05  CH-LAWCODE                 PIC 99.
       COPY "LIBLP/LP01SPA_AREA.CPY" REPLACING LEADING "SPA-" BY "CH-"
                                     LEADING "AEXPR" BY "CEXPR"
                                     LEADING "AASTP" BY "CASTP"
                                     LEADING "ACTBL" BY "CCTBL"
                                     LEADING "ACSTP" BY "CCSTP"
                                     LEADING "AETBL" BY "CETBL"
                                     LEADING "AESTP" BY "CESTP".
      * SIZE=2733
       COPY "LIBLP/LP01SPB_AREA.CPY" REPLACING LEADING "SPB-" BY "CH-"
                                        LEADING "BILEV" BY "CILEV"
                                        LEADING "BISTP" BY "CISTP"
                                        LEADING "BIVER" BY "CIVER".
      * SIZE=2231
       COPY "LIBLP/LP01SPC_AREA.CPY" REPLACING LEADING "SPC-" BY "CH-".

      *=================================================================
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

      *=================================================================
      *    L I N K A G E - S E C T I O N
      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       01  LINK-OR-REC        PIC X(SP-SIZE).
       01  LINK-CH-REC        PIC X(SP-SIZE).


      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME LINK-OR-REC LINK-CH-REC EXIT-PATHNAME.

      /*************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE LINK-OR-REC TO OR-REC.
           MOVE LINK-CH-REC TO CH-REC.

           PERFORM UPDATE-CHANGES.

           GO TO END-ROUTINE.


      *************************************************
       UPDATE-CHANGES SECTION.
      *************************************************
      *COMPARE ORIGINAL REC TO SP-REC

      *EFFECTIVE  DATE 
           IF OR-EFFDATE NOT = CH-EFFDATE
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "EFF DATE   "       TO MEMO-FIELD
              MOVE OR-EFFDATE          TO MEMO-OR-DATE
              MOVE CH-EFFDATE          TO MEMO-CH-DATE
              PERFORM CREATE-SM-MEMO.

      * PAGE 1

      *EXPIRATION DATE 
           IF OR-EXPDATE NOT = CH-EXPDATE
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "EXP DATE   "       TO MEMO-FIELD
              MOVE "01#01"             TO MEMO-PG-FLD
              MOVE OR-EXPDATE          TO MEMO-OR-DATE
              MOVE CH-EXPDATE          TO MEMO-CH-DATE
              PERFORM CREATE-SM-MEMO.


      *JUDGMENT FORMULA 
           IF OR-JUDGFRMLA NOT = CH-JUDGFRMLA
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "JDG FRMLA  "  TO MEMO-FIELD
              MOVE "01#02"             TO MEMO-PG-FLD
              MOVE OR-JUDGFRMLA TO MEMO-OR-CHAR
              MOVE CH-JUDGFRMLA TO MEMO-CH-CHAR
              PERFORM CREATE-SM-MEMO.

      *JUDGMENT RATE
           IF OR-JUDGRATE NOT = CH-JUDGRATE
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "JDG RATE   "       TO MEMO-FIELD
              MOVE "01#01"            TO MEMO-PG-FLD
              MOVE OR-JUDGRATE TO MEMO-OR-DEC
              MOVE CH-JUDGRATE TO MEMO-CH-DEC
              PERFORM CREATE-SM-MEMO.

      *JUDGMENT YEAR TYPE 
           IF OR-JUDGYRTYPE NOT = CH-JUDGYRTYPE
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "JDG YR TYP "       TO MEMO-FIELD
              MOVE "01#02"            TO MEMO-PG-FLD
              MOVE OR-JUDGYRTYPE TO MEMO-OR-NUM
              MOVE CH-JUDGYRTYPE TO MEMO-CH-NUM
              PERFORM CREATE-SM-MEMO.

      *JUDGMENT INTEREST METHOD 
           IF OR-JUDGINTMETH NOT = CH-JUDGINTMETH
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "JDG INT MET"       TO MEMO-FIELD
              MOVE "01#02"            TO MEMO-PG-FLD
              MOVE OR-JUDGINTMETH TO MEMO-OR-CHAR
              MOVE CH-JUDGINTMETH TO MEMO-CH-CHAR
              PERFORM CREATE-SM-MEMO.



      *1098 FORMULA - - - - - - - - - - - - - - - - - - - - - - - -
            IF OR-1098FRMLA NOT = CH-1098FRMLA
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "1098 FRML  "       TO MEMO-FIELD
               MOVE "01#03"             TO MEMO-PG-FLD
               MOVE OR-1098FRMLA        TO MEMO-OR-CHAR
               MOVE CH-1098FRMLA        TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *INCLUDE LATE CHARGES IN 1098 INTEREST- - - - - - - - - - - -
           IF OR-1098-INC-LC NOT = CH-1098-INC-LC
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "INC LC 1098"       TO MEMO-FIELD
               MOVE "01#03"             TO MEMO-PG-FLD
               MOVE OR-1098-INC-LC      TO MEMO-OR-CHAR
               MOVE CH-1098-INC-LC      TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *DISCLOSURE FORMULA - - - - - - - - - - - - - - - - - - - - -
            IF OR-DISFRMLA NOT = CH-DISFRMLA
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DISCLOSURE "       TO MEMO-FIELD
               MOVE "01#04"             TO MEMO-PG-FLD
               MOVE  OR-DISFRMLA        TO MEMO-OR-CHAR
               MOVE  CH-DISFRMLA        TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

            IF OR-DISFRMLA-SPAY-FG NOT = CH-DISFRMLA-SPAY-FG
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DISCLOS OPT"       TO MEMO-FIELD
               MOVE "01#04"             TO MEMO-PG-FLD
               MOVE OR-DISFRMLA-SPAY-FG TO MEMO-OR-CHAR
               MOVE CH-DISFRMLA-SPAY-FG TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.


      *CONTRACTUAL FORMULA- - - - - - - - - - - - - - - - - - - - -
            IF OR-CONTRFRMLA NOT = CH-CONTRFRMLA
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CONT FRMLA "       TO MEMO-FIELD
               MOVE "01#05"             TO MEMO-PG-FLD
               MOVE OR-CONTRFRMLA       TO MEMO-OR-CHAR
               MOVE CH-CONTRFRMLA       TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *BANKRULE  -  - - - - - - - - - - - - - - - - - - - - - - - -
            IF OR-BANKRULE NOT = CH-BANKRULE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "BANK RULE  "       TO MEMO-FIELD
               MOVE "01#06"             TO MEMO-PG-FLD
               MOVE OR-BANKRULE         TO MEMO-OR-CHAR
               MOVE CH-BANKRULE         TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *CONTRACTUAL PAID THRU- - - - - - - - - - - - - - - - - - - -
            IF OR-CONTPDTH NOT = CH-CONTPDTH
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CON PDTH % "       TO MEMO-FIELD
               MOVE "01#07"             TO MEMO-PG-FLD
               MOVE  OR-CONTPDTH        TO MEMO-OR-NUM
               MOVE  CH-CONTPDTH        TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *PAY FREQUENCY- - - - - - - - - - - - - - - - - - - - - - - -
            IF OR-BANKUNITPER-CD NOT = CH-BANKUNITPER-CD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "PAY CCD FRQ"       TO MEMO-FIELD
               MOVE "01#08"             TO MEMO-PG-FLD
               MOVE OR-BANKUNITPER-CD   TO MEMO-OR-CHAR
               MOVE CH-BANKUNITPER-CD   TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *TRW BANK RULE-(HOW BEING REPORTED TO CREDIT BUREAU - - - - -
            IF OR-TRWBANKRULE NOT = CH-TRWBANKRULE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "TRW BANK RU"       TO MEMO-FIELD
               MOVE "01#09"             TO MEMO-PG-FLD
               MOVE OR-TRWBANKRULE      TO MEMO-OR-CHAR
               MOVE CH-TRWBANKRULE      TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *CREDIT BUREAU REPORTING CONTRACTUAL PAID THRU- - - - - - - -
            IF OR-TRWCONTPDTH NOT = CH-TRWCONTPDTH
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "TRW PDTH % "       TO MEMO-FIELD
               MOVE "01#10"             TO MEMO-PG-FLD
               MOVE OR-TRWCONTPDTH TO MEMO-OR-NUM
               MOVE CH-TRWCONTPDTH TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *TRW UNIT PERIOD CODE - - - - - - - - - - - - - - - - - - - -
            IF OR-TRWBANKUNITPER-CD NOT = CH-TRWBANKUNITPER-CD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "TRW PY FREQ"       TO MEMO-FIELD
               MOVE "01#11"             TO MEMO-PG-FLD
               MOVE OR-TRWBANKUNITPER-CD TO MEMO-OR-CHAR
               MOVE CH-TRWBANKUNITPER-CD TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *RECENCY FORMULA- - - - - - - - - - - - - - - - - - - - - - -
            IF OR-RECFRMLA NOT = CH-RECFRMLA
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "REC DLQ FRM"       TO MEMO-FIELD
               MOVE "01#12"             TO MEMO-PG-FLD
               MOVE OR-RECFRMLA         TO MEMO-OR-CHAR
               MOVE CH-RECFRMLA         TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *RECENCY PAY CODE - - - - - - - - - - - - - - - - - - - - - -
            IF OR-RECPAYCD NOT = CH-RECPAYCD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "REC PY CD  "       TO MEMO-FIELD
               MOVE "01#12"             TO MEMO-PG-FLD
               MOVE OR-RECPAYCD         TO MEMO-OR-CHAR
               MOVE CH-RECPAYCD         TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *RECENCY PAID THRU- - - - - - - - - - - - - - - - - - - - - -
            IF OR-RECPDTH NOT = CH-RECPDTH
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "REC PDTH % "       TO MEMO-FIELD
               MOVE "01#12"             TO MEMO-PG-FLD
               MOVE OR-RECPDTH          TO MEMO-OR-NUM
               MOVE CH-RECPDTH          TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.


      *LATE CHARGE FORMULA- - - - - - - - - - - - - - - - - - - - -
           IF OR-LCFRMLA NOT = CH-LCFRMLA
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "L/C FRMLA  "       TO MEMO-FIELD
               MOVE "01#16"             TO MEMO-PG-FLD
               MOVE OR-LCFRMLA          TO MEMO-OR-CHAR
               MOVE CH-LCFRMLA          TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *LATE CHARGE CODE - - - - - - - - - - - - - - - - - - - - - -
           IF OR-LCTYPE NOT = CH-LCTYPE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "L/C CODE   "       TO MEMO-FIELD
               MOVE "01#17"             TO MEMO-PG-FLD
               MOVE OR-LCTYPE           TO MEMO-OR-CHAR
               MOVE CH-LCTYPE           TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *LATE CHARGE RATE - - - - - - - - - - - - - - - - - - - - - -
           IF OR-LCRATE NOT = CH-LCRATE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "L/C RATE   "       TO MEMO-FIELD
               MOVE "01#18"             TO MEMO-PG-FLD
               MOVE OR-LCRATE           TO MEMO-OR-DEC4
               MOVE CH-LCRATE           TO MEMO-CH-DEC4
               PERFORM CREATE-SM-MEMO.

      *LATE CHARGE MINIMUM- - - - - - - - - - - - - - - - - - - -
           IF OR-LCMINCHG NOT = CH-LCMINCHG
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "L/C MINIMUM"       TO MEMO-FIELD
               MOVE "01#19"             TO MEMO-PG-FLD
               MOVE OR-LCMINCHG         TO MEMO-OR-DEC
               MOVE CH-LCMINCHG         TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *LATE CHARGE MAXIMUM- - - - - - - - - - - - - - - - - - - - -
           IF OR-LCMAXAMT NOT = CH-LCMAXAMT
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "L/C MAXIMUM"       TO MEMO-FIELD
               MOVE "01#20"             TO MEMO-PG-FLD
               MOVE OR-LCMAXAMT         TO MEMO-OR-DEC
               MOVE CH-LCMAXAMT         TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *LATE CHARGE GRACE DAYS - - - - - - - - - - - - - - - - - - -
           IF OR-LCGRACE NOT = CH-LCGRACE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "L/C GRACE  "       TO MEMO-FIELD
               MOVE "01#21"             TO MEMO-PG-FLD
               MOVE OR-LCGRACE          TO MEMO-OR-NUM
               MOVE CH-LCGRACE          TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *GRACE DAYS CODE (CONSIDER WEEKENDS OR HOLIDAYS)- - - - - - -
           IF OR-LCGRACECD NOT = CH-LCGRACECD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "GRACE CODE "       TO MEMO-FIELD
               MOVE "01#21"             TO MEMO-PG-FLD
               MOVE OR-LCGRACECD        TO MEMO-OR-CHAR
               MOVE CH-LCGRACECD        TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.


      *LATE CHARGE YEAR TYPE- - - - - - - - - - - - - - - - - - - -
           IF OR-LCYRTYPE NOT = CH-LCYRTYPE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "L/C YR TYPE"       TO MEMO-FIELD
               MOVE "01#22"             TO MEMO-PG-FLD
               MOVE OR-LCYRTYPE         TO MEMO-OR-NUM
               MOVE CH-LCYRTYPE         TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *DELINQUENCY FACTOR - - - - - - - - - - - - - - - - - - - - -
           IF OR-LCDELFAC NOT = CH-LCDELFAC
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "L/C DELQ FA"       TO MEMO-FIELD
               MOVE "01#23"             TO MEMO-PG-FLD
               MOVE OR-LCDELFAC         TO MEMO-OR-DEC
               MOVE CH-LCDELFAC         TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *USED INTERNALLY - - - - - - - - - - - - - - - - - - - - - -
      *    IF OR-LC-CODE NOT = CH-LC-CODE
      *        MOVE SPACES              TO MEMO-OR-CHAR
      *        MOVE SPACES              TO MEMO-CH-CHAR
      *        MOVE "LC-CODE         "  TO MEMO-FIELD
      *        MOVE "00#00"             TO MEMO-PG-FLD
      *        MOVE OR-LC-CODE          TO MEMO-OR-CHAR
      *        MOVE CH-LC-CODE          TO MEMO-CH-CHAR
      *        PERFORM CREATE-SM-MEMO.

      *PAYCODE FREQUENCY LATE CHARGE- - - - - - - - - - - - - - - -
           IF OR-LCUNITPER-CD NOT = CH-LCUNITPER-CD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "L/C PAY FRQ"       TO MEMO-FIELD
               MOVE "01#24"             TO MEMO-PG-FLD
               MOVE OR-LCUNITPER-CD     TO MEMO-OR-CHAR
               MOVE CH-LCUNITPER-CD     TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF OR-LC-ON-JUDGEMENTS NOT = CH-LC-ON-JUDGEMENTS
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "LC ON JUDGMT"      TO MEMO-FIELD
               MOVE "01#25"             TO MEMO-PG-FLD
               MOVE OR-LC-ON-JUDGEMENTS TO MEMO-OR-CHAR
               MOVE CH-LC-ON-JUDGEMENTS TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-AUTO-ASSESS-LCHG NOT = CH-AUTO-ASSESS-LCHG
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "AUTO ASS LCG"      TO MEMO-FIELD
               MOVE "01#26"             TO MEMO-PG-FLD
               MOVE OR-AUTO-ASSESS-LCHG TO MEMO-OR-CHAR
               MOVE CH-AUTO-ASSESS-LCHG TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-RENEWAL-EDIT NOT = CH-RENEWAL-EDIT
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "RENEWAL EDIT "     TO MEMO-FIELD
               MOVE "01#27"             TO MEMO-PG-FLD
               MOVE OR-RENEWAL-EDIT     TO MEMO-OR-CHAR
               MOVE CH-RENEWAL-EDIT     TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-INSURANCE-WRITTEN NOT = CH-INSURANCE-WRITTEN
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "INS WRITTEN "      TO MEMO-FIELD
               MOVE "01#28"             TO MEMO-PG-FLD
               MOVE OR-INSURANCE-WRITTEN TO MEMO-OR-CHAR
               MOVE CH-INSURANCE-WRITTEN TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-UNITPER-CD NOT = CH-UNITPER-CD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "UNITPER CODE "     TO MEMO-FIELD
               MOVE "01#29"             TO MEMO-PG-FLD
               MOVE OR-UNITPER-CD       TO MEMO-OR-CHAR
               MOVE CH-UNITPER-CD       TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-DESC NOT = CH-DESC
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "SPR DESC   "       TO MEMO-FIELD
               MOVE "01#30"             TO MEMO-PG-FLD
               MOVE OR-DESC       TO MEMO-OR-CHAR
               MOVE CH-DESC       TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      * PAGE 2

      *DEFERMENT FORMULA- - - - - - - - - - - - - - - - - - - - - -
           IF OR-DEFFRMLA NOT = CH-DEFFRMLA
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DEFMNT FRML"       TO MEMO-FIELD
               MOVE "02#01"             TO MEMO-PG-FLD
               MOVE OR-DEFFRMLA         TO MEMO-OR-NUM
               MOVE CH-DEFFRMLA         TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *DEFERMENT EXCEPTION- - - - - - - - - - - - - - - - - - - - -
           IF OR-DEFEXCEPT NOT = CH-DEFEXCEPT
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DEFMNT EXC "       TO MEMO-FIELD
               MOVE "02#01"             TO MEMO-PG-FLD
               MOVE OR-DEFEXCEPT        TO MEMO-OR-CHAR
               MOVE CH-DEFEXCEPT        TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-DEFUNITPER-CD NOT = CH-DEFUNITPER-CD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DEFMNT FRQ "       TO MEMO-FIELD
               MOVE "02#01"             TO MEMO-PG-FLD
               MOVE OR-DEFUNITPER-CD    TO MEMO-OR-CHAR
               MOVE CH-DEFUNITPER-CD    TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *DEFERMENT BASE - - - - - - - - - - - - - - - - - - - - - - -
           IF OR-DEFBASE NOT = CH-DEFBASE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DEFMNT BASE"       TO MEMO-FIELD
               MOVE "02#02"             TO MEMO-PG-FLD
               MOVE OR-DEFBASE TO MEMO-OR-NUM
               MOVE CH-DEFBASE TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *DEFERMENT PERCENT CODE - - - - - - - - - - - - - - - - - - -
           IF OR-DEFPERCD NOT = CH-DEFPERCD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DEFMNT % CD"       TO MEMO-FIELD
               MOVE "02#03"             TO MEMO-PG-FLD
               MOVE OR-DEFPERCD         TO MEMO-OR-NUM
               MOVE CH-DEFPERCD         TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *DEFERMENT WORKER - - - - - - - - - - - - - - - - - - - - - -
           IF OR-DEFWORKER NOT = CH-DEFWORKER
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DEFMNT WORK"       TO MEMO-FIELD
               MOVE "02#04"             TO MEMO-PG-FLD
               MOVE OR-DEFWORKER        TO MEMO-OR-DEC4
               MOVE CH-DEFWORKER        TO MEMO-CH-DEC4
               PERFORM CREATE-SM-MEMO.

      *DEFERMENT MINIMUM- - - - - - - - - - - - - - - - - - - - - -
           IF OR-DEFMIN NOT = CH-DEFMIN
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DEFMNT MIN "       TO MEMO-FIELD
               MOVE "02#05"             TO MEMO-PG-FLD
               MOVE OR-DEFMIN           TO MEMO-OR-DEC
               MOVE CH-DEFMIN           TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *DEFERMENT MAXIMUM- - - - - - - - - - - - - - - - - - - - - -
           IF OR-DEFMAX NOT = CH-DEFMAX
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DEFMNT MAX "       TO MEMO-FIELD
               MOVE "02#06"             TO MEMO-PG-FLD
               MOVE OR-DEFMAX           TO MEMO-OR-DEC
               MOVE CH-DEFMAX           TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.


      *DEFERMENT POLICY CODE- - - - - - - - - - - - - - - - - - - -
           IF OR-DEFPOLCD NOT = CH-DEFPOLCD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DEFMNT POL "       TO MEMO-FIELD
               MOVE "02#07"             TO MEMO-PG-FLD
               MOVE OR-DEFPOLCD         TO MEMO-OR-CHAR
               MOVE CH-DEFPOLCD         TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *SP-DEFPOLMX      OCCURS 3

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
               COMPUTE FIELD-NO = 7 + SUB
               IF OR-DEFPOLMX(SUB) NOT =  CH-DEFPOLMX(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE "DEFMNT MAX "       TO MEMO-FIELD
                 MOVE  02                 TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO            TO MEMO-FLD-NUM
                 MOVE OR-DEFPOLMX(SUB)    TO MEMO-OR-DEC
                 MOVE CH-DEFPOLMX(SUB)    TO MEMO-CH-DEC
                 PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.


      *FORM TABLE  - - - - - -  - - - - - - - - - - - - - - - - - -
      *FORM NUMBER
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
               COMPUTE FIELD-NO = 13 + SUB
               IF OR-PRFORM(SUB) NOT =  CH-PRFORM(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE "FORM #     "       TO MEMO-FIELD
                 MOVE  02                TO MEMO-PG
                 MOVE "#"                TO MEMO-SEP
                 MOVE FIELD-NO               TO MEMO-FLD-NUM
                 MOVE OR-PRFORM(SUB)     TO MEMO-OR-NUM
                 MOVE CH-PRFORM(SUB)     TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *AUTO REQUEST PRINT FLAG
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
               COMPUTE FIELD-NO = 13 + SUB
               IF OR-PRFLAG(SUB) NOT =  CH-PRFLAG(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE "AUTO REQ   "       TO MEMO-FIELD
                 MOVE  02                 TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO            TO MEMO-FLD-NUM
                 MOVE OR-PRFLAG(SUB)      TO MEMO-OR-CHAR
                 MOVE CH-PRFLAG(SUB)     TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *IMMEDIATE RECEIPT PRINT  - - - - - - - - - - - - - - - - - -
           IF OR-IMED-REC NOT = CH-IMED-REC
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "IMMED RECPT"       TO MEMO-FIELD
               MOVE "02#24"             TO MEMO-PG-FLD
               MOVE OR-IMED-REC         TO MEMO-OR-CHAR
               MOVE CH-IMED-REC         TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-INTERIM-COUPON NOT = CH-INTERIM-COUPON
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "INTRIM COUPN"      TO MEMO-FIELD
               MOVE "02#25"             TO MEMO-PG-FLD
               MOVE OR-INTERIM-COUPON TO MEMO-OR-CHAR
               MOVE CH-INTERIM-COUPON TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      * PAGE 3

      *INTEREST METHOD - - - -  - - - - - - - - - - - - - - - - - -
           IF OR-INTMETHOD NOT = CH-INTMETHOD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "INT METHOD "       TO MEMO-FIELD
               MOVE "03#01"             TO MEMO-PG-FLD
               MOVE OR-INTMETHOD        TO MEMO-OR-CHAR
               MOVE CH-INTMETHOD        TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *INTEREST YEAR TYPE- - -  - - - - - - - - - - - - - - - - - -
           IF OR-YEARTYPE NOT = CH-YEARTYPE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "INT YR TYPE"       TO MEMO-FIELD
               MOVE "03#02"             TO MEMO-PG-FLD
               MOVE OR-YEARTYPE         TO MEMO-OR-NUM
               MOVE CH-YEARTYPE         TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *1ST PAYMENT EXTENTION FORMULA- - - - - - - - - - - - - - - -
           IF OR-PYEXFRMLA NOT = CH-PYEXFRMLA
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "1ST PAY EXT"       TO MEMO-FIELD
               MOVE "03#03"             TO MEMO-PG-FLD
               MOVE OR-PYEXFRMLA        TO MEMO-OR-NUM
               MOVE CH-PYEXFRMLA        TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *1ST PAYMENT EXTENTION CODE - - - - - - - - - - - - - - - - -
           IF OR-PYEXSPCODE NOT = CH-PYEXSPCODE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "1ST PAY CD "       TO MEMO-FIELD
               MOVE "03#04"             TO MEMO-PG-FLD
               MOVE OR-PYEXSPCODE       TO MEMO-OR-NUM
               MOVE CH-PYEXSPCODE       TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *1ST PAYMENT EXTENTION YEAR TYPE- - - - - - - - - - - - - - -
           IF OR-PYEXYEARTP NOT = CH-PYEXYEARTP
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "1ST PAY YR "       TO MEMO-FIELD
               MOVE "03#05"             TO MEMO-PG-FLD
               MOVE OR-PYEXYEARTP       TO MEMO-OR-NUM
               MOVE CH-PYEXYEARTP       TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *1ST PAYMENT EXTENTION FROM DATE (INT START  OR LOAN DATE)- -
           IF OR-PYEXFMDATE NOT = CH-PYEXFMDATE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "1ST PY FROM"      TO MEMO-FIELD
               MOVE "03#06"             TO MEMO-PG-FLD
               MOVE OR-PYEXFMDATE       TO MEMO-OR-CHAR
               MOVE CH-PYEXFMDATE       TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *1ST PAYMENT EXTENTION BASE CODE (DAYS OR MONTH)- - - - - - -
           IF OR-PYEXBASECD NOT = CH-PYEXBASECD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "1ST PY BASE"       TO MEMO-FIELD
               MOVE "03#07"             TO MEMO-PG-FLD
               MOVE OR-PYEXBASECD       TO MEMO-OR-CHAR
               MOVE CH-PYEXBASECD       TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *1ST PAYMENT EXTENTION BASE - - - - - - - - - - - - - - - - -
           IF OR-PYEXBASE NOT = CH-PYEXBASE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "1ST PY BASE"       TO MEMO-FIELD
               MOVE "03#08"             TO MEMO-PG-FLD
               MOVE OR-PYEXBASE TO MEMO-OR-NUM
               MOVE CH-PYEXBASE TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *1ST PAYMENT EXTENTION DAYS - - - - - - - - - - - - - - - - -
           IF OR-PYEXDAYS NOT = CH-PYEXDAYS
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "1ST PY DAYS"       TO MEMO-FIELD
               MOVE "03#09"             TO MEMO-PG-FLD
               MOVE OR-PYEXDAYS         TO MEMO-OR-NUM
               MOVE CH-PYEXDAYS         TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *MAX APR - - - - - - - -  - - - - - - - - - - - - - - - - - -
           IF OR-MAXAPR-FRMLA NOT = CH-MAXAPR-FRMLA
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "MAX APR FRM"       TO MEMO-FIELD
               MOVE "03#10"             TO MEMO-PG-FLD
               MOVE OR-MAXAPR-FRMLA     TO MEMO-OR-NUM
               MOVE CH-MAXAPR-FRMLA     TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *MINIMUM S/F FORMULA - -  - - - - - - - - - - - - - - - - - -
           IF OR-MINSF-FRMLA NOT = CH-MINSF-FRMLA
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "MIN S/F FRM"       TO MEMO-FIELD
               MOVE "03#11"             TO MEMO-PG-FLD
               MOVE OR-MINSF-FRMLA      TO MEMO-OR-NUM
               MOVE CH-MINSF-FRMLA      TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *MINIMUM S/F CHARGE  - -  - - - - - - - - - - - - - - - - - -
           IF OR-MINSF-CHG NOT = CH-MINSF-CHG
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "MIN S/F CHG"       TO MEMO-FIELD
               MOVE "03#12"             TO MEMO-PG-FLD
               MOVE OR-MINSF-CHG        TO MEMO-OR-DEC
               MOVE CH-MINSF-CHG        TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *SERVICE CHARGE FORMULA-  - - - - - - - - - - - - - - - - - -
           IF OR-SCFRMLA NOT = CH-SCFRMLA
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "S/C FRMLA  "       TO MEMO-FIELD
               MOVE "03#13"             TO MEMO-PG-FLD
               MOVE OR-SCFRMLA          TO MEMO-OR-NUM
               MOVE CH-SCFRMLA          TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *SERVICE CHARGE RATE - -  - - - - - - - - - - - - - - - - - -
           IF OR-SCRATE NOT = CH-SCRATE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "S/C RATE   "       TO MEMO-FIELD
               MOVE "03#14"             TO MEMO-PG-FLD
               MOVE OR-SCRATE           TO MEMO-OR-DEC
               MOVE CH-SCRATE           TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *SERVICE CHARGE MIMIMUM-  - - - - - - - - - - - - - - - - - -
           IF OR-SCMIN NOT = CH-SCMIN
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "S/C MIN    "       TO MEMO-FIELD
               MOVE "03#15"             TO MEMO-PG-FLD
               MOVE OR-SCMIN            TO MEMO-OR-DEC
               MOVE CH-SCMIN            TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *SERVICE CHARGE MAXIMUM-  - - - - - - - - - - - - - - - - - -
           IF OR-SCMAX NOT = CH-SCMAX
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "S/C MAX    "       TO MEMO-FIELD
               MOVE "03#16"             TO MEMO-PG-FLD
               MOVE OR-SCMAX            TO MEMO-OR-DEC
               MOVE CH-SCMAX            TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *MAINTENANCE FEE FORMULA  - - - - - - - - - - - - - - - - - -
           IF OR-MFFRMLA NOT = CH-MFFRMLA
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "M/F FRMLA  "       TO MEMO-FIELD
               MOVE "03#17"             TO MEMO-PG-FLD
               MOVE OR-MFFRMLA          TO MEMO-OR-NUM
               MOVE CH-MFFRMLA          TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

           IF OR-ROUND-MFEE NOT = CH-ROUND-MFEE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "ROUND MFEE  "      TO MEMO-FIELD
               MOVE "03#17"             TO MEMO-PG-FLD
               MOVE OR-ROUND-MFEE       TO MEMO-OR-CHAR
               MOVE CH-ROUND-MFEE       TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.


      *MAINTENANCE FEE RATE- -  - - - - - - - - - - - - - - - - - -
           IF OR-MFRATE NOT = CH-MFRATE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "M/F RATE   "       TO MEMO-FIELD
               MOVE "03#18"             TO MEMO-PG-FLD
               MOVE OR-MFRATE           TO MEMO-OR-DEC
               MOVE CH-MFRATE           TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *MAINTENANCE FEE MINIMUM - -  - - - - - - - - - - - - - - - - - -
           IF OR-MFMIN NOT = CH-MFMIN
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "M/F MINIMUM"       TO MEMO-FIELD
               MOVE "03#19"             TO MEMO-PG-FLD
               MOVE OR-MFMIN            TO MEMO-OR-DEC
               MOVE CH-MFMIN            TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *MAINTENANCE FEE MAXIMUM  - - - - - - - - - - - - - - - - - -
           IF OR-MFMAX NOT = CH-MFMAX
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "M/F MAXIMUM"       TO MEMO-FIELD
               MOVE "03#20"             TO MEMO-PG-FLD
               MOVE OR-MFMAX            TO MEMO-OR-DEC
               MOVE CH-MFMAX            TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *ACCELERATION FORMULA - - - - - - - - - - - - - - - - - - - -
           IF OR-ACC-FRMLA NOT = CH-ACC-FRMLA
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "ACCEL FRM  "       TO MEMO-FIELD
              MOVE "03#21"             TO MEMO-PG-FLD
              MOVE OR-ACC-FRMLA TO MEMO-OR-CHAR
              MOVE CH-ACC-FRMLA TO MEMO-CH-CHAR
              PERFORM CREATE-SM-MEMO.

      *ACCELERATION YRTYPE- - - - - - - - - - - - - - - - - - - - -
           IF OR-ACC-YRTYPE NOT = CH-ACC-YRTYPE
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "ACCEL YR TY"       TO MEMO-FIELD
              MOVE "03#22"            TO MEMO-PG-FLD
              MOVE OR-ACC-YRTYPE TO MEMO-OR-NUM
              MOVE CH-ACC-YRTYPE TO MEMO-CH-NUM
              PERFORM CREATE-SM-MEMO.

      *ACCELERATION INTEREST METHOD - - - - - - - - - - - - - - - -
           IF OR-ACC-INTMETH NOT = CH-ACC-INTMETH
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "ACC INT MET"       TO MEMO-FIELD
              MOVE "03#23"            TO MEMO-PG-FLD
              MOVE OR-ACC-INTMETH TO MEMO-OR-CHAR
              MOVE CH-ACC-INTMETH TO MEMO-CH-CHAR
              PERFORM CREATE-SM-MEMO.

      *    SP-ACC-RATETBL                   OCCURS 5
      *--ACC-RATE
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               COMPUTE FIELD-NO = 23 + SUB
             IF OR-ACC-RATE(SUB) NOT =  CH-ACC-RATE(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE "ACCL RATE  "       TO MEMO-FIELD
                 MOVE  03                 TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO                TO MEMO-FLD-NUM
                 MOVE OR-ACC-RATE(SUB)  TO MEMO-OR-DEC3
                 MOVE CH-ACC-RATE(SUB)  TO MEMO-CH-DEC3
                 PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.

      *   ACC-BREAK
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
             COMPUTE FIELD-NO = 23 + SUB
             IF OR-ACC-BREAK(SUB) NOT =  CH-ACC-BREAK(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE "ACCEL BRK "        TO MEMO-FIELD
                 MOVE  03                 TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO                TO MEMO-FLD-NUM
                 MOVE OR-ACC-BREAK(SUB)   TO MEMO-OR-DEC
                 MOVE CH-ACC-BREAK(SUB)   TO MEMO-CH-DEC
                 PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.

      * PAGE 4

      *SPECIAL FORMULA - - - -  - - - - - - - - - - - - - - - - - -
           IF OR-CAL-SPFRMLA NOT = CH-CAL-SPFRMLA
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "SPECIAL FRM"       TO MEMO-FIELD
               MOVE "04#01"             TO MEMO-PG-FLD
               MOVE OR-CAL-SPFRMLA      TO MEMO-OR-NUM
               MOVE CH-CAL-SPFRMLA      TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *TABLE RATES - -(SP-CAL-TABLE)- - - - - - - - - - - - - - - -
      *RATE TYPE
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF OR-CAL-RATETYPE(SUB) NOT = CH-CAL-RATETYPE(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE "RATE TYPE  "       TO MEMO-FIELD
                 MOVE "04"                TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 IF SUB = 1
                    MOVE 02 TO FIELD-NO
                 ELSE
                    MOVE 17 TO FIELD-NO
                 END-IF
                 MOVE FIELD-NO             TO MEMO-FLD
                 MOVE OR-CAL-RATETYPE(SUB) TO MEMO-OR-CHAR
                 MOVE CH-CAL-RATETYPE(SUB) TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *BREAK TYPE
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF OR-CAL-BREAKTYPE(SUB) NOT = CH-CAL-BREAKTYPE(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE "BREAK TYPE "       TO MEMO-FIELD
                 MOVE  04                 TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 IF SUB = 1
                    MOVE 03 TO FIELD-NO
                 ELSE
                    MOVE 18 TO FIELD-NO
                 END-IF
                 MOVE FIELD-NO                TO MEMO-FLD-NUM
                 MOVE OR-CAL-BREAKTYPE(SUB) TO MEMO-OR-CHAR
                 MOVE CH-CAL-BREAKTYPE(SUB) TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *CHARGEABLE CODE
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF OR-CAL-CHGABLE(SUB) NOT = CH-CAL-CHGABLE(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE "CHARGEABLE "       TO MEMO-FIELD
                 MOVE  04                 TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 IF SUB = 1
                    MOVE 4 TO FIELD-NO
                 ELSE
                    MOVE 19 TO FIELD-NO
                 END-IF
                 MOVE FIELD-NO                TO MEMO-FLD
                 MOVE OR-CAL-CHGABLE(SUB) TO MEMO-OR-CHAR
                 MOVE CH-CAL-CHGABLE(SUB) TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.
      *%%%%%%    SP-CAL-NORATES
      *TABLE FORMULA- - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF OR-CAL-TBFRMLA(SUB) NOT = CH-CAL-TBFRMLA(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE "TABLE FRMLA"       TO MEMO-FIELD
                 MOVE  04                 TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 IF SUB = 1
                    MOVE 5 TO FIELD-NO
                 ELSE
                    MOVE 20 TO FIELD-NO
                 END-IF
                 MOVE FIELD-NO                TO MEMO-FLD
                 MOVE OR-CAL-TBFRMLA(SUB) TO MEMO-OR-NUM
                 MOVE CH-CAL-TBFRMLA(SUB) TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *RATE TABLE- - (RATE, AMT/TERM- - - - - - - - - - - - - - - - - -
      *SP-CAL-RATETBL          OCCURS 5

      *RATE- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 5
              IF SUB = 1
                  COMPUTE FIELD-NO = 5 + S1
              ELSE
                  COMPUTE FIELD-NO = 20 + S1
              END-IF

              IF OR-CAL-RATE(SUB,S1) NOT = CH-CAL-RATE(SUB,S1)
                  MOVE SPACES              TO MEMO-OR-CHAR
                  MOVE SPACES              TO MEMO-CH-CHAR
                  MOVE "RATE       "       TO MEMO-FIELD
                  MOVE  04                 TO MEMO-PG
                  MOVE "#"                 TO MEMO-SEP
                  MOVE FIELD-NO            TO MEMO-FLD
                  MOVE OR-CAL-RATE(SUB,S1) TO MEMO-OR-DEC3
                  MOVE CH-CAL-RATE(SUB,S1) TO MEMO-CH-DEC3
                  PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *BREAK- - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 5
              IF SUB = 1
                  COMPUTE FIELD-NO = 5 + S1
              ELSE
                  COMPUTE FIELD-NO = 20 + S1
              END-IF

              IF OR-CAL-BREAK(SUB,S1) NOT = CH-CAL-BREAK(SUB,S1)
                  MOVE SPACES              TO MEMO-OR-CHAR
                  MOVE SPACES              TO MEMO-CH-CHAR
                  MOVE "AMT / TERM "       TO MEMO-FIELD
                  MOVE  04                 TO MEMO-PG
                  MOVE "#"                 TO MEMO-SEP
                  MOVE FIELD-NO            TO MEMO-FLD
                  MOVE OR-CAL-BREAK(SUB,S1) TO MEMO-OR-DEC
                  MOVE CH-CAL-BREAK(SUB,S1) TO MEMO-CH-DEC
                  PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.


      *EDIT TABLE- - - - - - - - - - - - - - - - - - - - - - - -
      * 03 SP-EDT-TABLE                       OCCURS 2
      *    05 SP-EDT-NOEDITS
      *       07  SP-EDT-TYPE-CODES.
      *    05  SP-EDT-STPTBL                  OCCURS 6

      *EDT-TYPE (R,A,T,M,P,V)- - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 6
              IF SUB = 1
                  COMPUTE FIELD-NO = 10 + S1
              ELSE
                  COMPUTE FIELD-NO = 25 + S1
              END-IF

              IF OR-EDT-TYPE(SUB,S1) NOT = CH-EDT-TYPE(SUB,S1)
                  MOVE SPACES              TO MEMO-OR-CHAR
                  MOVE SPACES              TO MEMO-CH-CHAR
                  MOVE "EDIT TYPE  "       TO MEMO-FIELD
                  MOVE  04                 TO MEMO-PG
                  MOVE "#"                 TO MEMO-SEP
                  MOVE FIELD-NO            TO MEMO-FLD
                  MOVE OR-EDT-TYPE(SUB,S1) TO MEMO-OR-CHAR
                  MOVE CH-EDT-TYPE(SUB,S1) TO MEMO-CH-CHAR
                  PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *EDT-HL (HIGH LOW)- - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 6
              IF SUB = 1
                  COMPUTE FIELD-NO = 10 + S1
              ELSE
                  COMPUTE FIELD-NO = 25 + S1
              END-IF
              IF OR-EDT-HL(SUB,S1) NOT = CH-EDT-HL(SUB,S1)
                  MOVE SPACES              TO MEMO-OR-CHAR
                  MOVE SPACES              TO MEMO-CH-CHAR
                  MOVE "HIGH / LOW "       TO MEMO-FIELD
                  MOVE  04                 TO MEMO-PG
                  MOVE "#"                 TO MEMO-SEP
                  MOVE FIELD-NO            TO MEMO-FLD
                  MOVE OR-EDT-HL(SUB,S1) TO MEMO-OR-CHAR
                  MOVE CH-EDT-HL(SUB,S1) TO MEMO-CH-CHAR
                  PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *EDT-COLL- - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 6
              IF SUB = 1
                  COMPUTE FIELD-NO = 10 + S1
              ELSE
                  COMPUTE FIELD-NO = 25 + S1
              END-IF
              IF OR-EDT-COLL(SUB,S1) NOT = CH-EDT-COLL(SUB,S1)
                  MOVE SPACES              TO MEMO-OR-CHAR
                  MOVE SPACES              TO MEMO-CH-CHAR
                  MOVE SPACES              TO MEMO-FIELD
                  MOVE "COLL       "       TO MEMO-FIELD
                  MOVE  04                 TO MEMO-PG
                  MOVE "#"                 TO MEMO-SEP
                  MOVE FIELD-NO            TO MEMO-FLD
                  MOVE OR-EDT-COLL(SUB,S1) TO MEMO-OR-CHAR
                  MOVE CH-EDT-COLL(SUB,S1) TO MEMO-CH-CHAR
                  PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *EDT-CD- - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 6
              IF SUB = 1
                  COMPUTE FIELD-NO = 10 + S1
              ELSE
                  COMPUTE FIELD-NO = 25 + S1
              END-IF
              IF OR-EDT-CD(SUB,S1) NOT = CH-EDT-CD(SUB,S1)
                  MOVE SPACES              TO MEMO-OR-CHAR
                  MOVE SPACES              TO MEMO-CH-CHAR
                  MOVE "CODE       "       TO MEMO-FIELD
                  MOVE  04                 TO MEMO-PG
                  MOVE "#"                 TO MEMO-SEP
                  MOVE FIELD-NO            TO MEMO-FLD
                  MOVE OR-EDT-CD(SUB,S1) TO MEMO-OR-CHAR
                  MOVE CH-EDT-CD(SUB,S1) TO MEMO-CH-CHAR
                  PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *EDT-RATE- - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 6
              IF SUB = 1
                  COMPUTE FIELD-NO = 10 + S1
              ELSE
                  COMPUTE FIELD-NO = 25 + S1
              END-IF
              IF OR-EDT-RATE(SUB,S1) NOT = CH-EDT-RATE(SUB,S1)
                  MOVE SPACES              TO MEMO-OR-CHAR
                  MOVE SPACES              TO MEMO-CH-CHAR
                  MOVE "RATE       "       TO MEMO-FIELD
                  MOVE  04                 TO MEMO-PG
                  MOVE "#"                 TO MEMO-SEP
                  MOVE FIELD-NO            TO MEMO-FLD
                  MOVE OR-EDT-RATE(SUB,S1) TO MEMO-OR-DEC
                  MOVE CH-EDT-RATE(SUB,S1) TO MEMO-CH-DEC
                  PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *RATETYPE - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 6
              IF SUB = 1
                  COMPUTE FIELD-NO = 10 + S1
              ELSE
                  COMPUTE FIELD-NO = 25 + S1
              END-IF
              IF OR-EDT-RATETYPE(SUB,S1) NOT = CH-EDT-RATETYPE(SUB,S1)
                  MOVE SPACES              TO MEMO-OR-CHAR
                  MOVE SPACES              TO MEMO-CH-CHAR
                  MOVE "RATE TYPE  "       TO MEMO-FIELD
                  MOVE  04                 TO MEMO-PG
                  MOVE "#"                 TO MEMO-SEP
                  MOVE FIELD-NO            TO MEMO-FLD
                  MOVE OR-EDT-RATETYPE(SUB,S1) TO MEMO-OR-CHAR
                  MOVE CH-EDT-RATETYPE(SUB,S1) TO MEMO-CH-CHAR
                  PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *EDT-AMT- - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 6
              IF SUB = 1
                  COMPUTE FIELD-NO = 10 + S1
              ELSE
                  COMPUTE FIELD-NO = 25 + S1
              END-IF
              IF OR-EDT-AMT(SUB,S1) NOT = CH-EDT-AMT(SUB,S1)
                  MOVE SPACES              TO MEMO-OR-CHAR
                  MOVE SPACES              TO MEMO-CH-CHAR
                  MOVE "AMOUNT     "       TO MEMO-FIELD
                  MOVE  04                 TO MEMO-PG
                  MOVE "#"                 TO MEMO-SEP
                  MOVE FIELD-NO            TO MEMO-FLD
                  MOVE OR-EDT-AMT(SUB,S1) TO MEMO-OR-DEC
                  MOVE CH-EDT-AMT(SUB,S1) TO MEMO-CH-DEC
                  PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *EDT-AMTYPE - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 6
              IF SUB = 1
                  COMPUTE FIELD-NO = 10 + S1
              ELSE
                  COMPUTE FIELD-NO = 25 + S1
              END-IF
              IF OR-EDT-AMTYPE(SUB,S1) NOT = CH-EDT-AMTYPE(SUB,S1)
                  MOVE SPACES              TO MEMO-OR-CHAR
                  MOVE SPACES              TO MEMO-CH-CHAR
                  MOVE "AMOUNT TYPE"       TO MEMO-FIELD
                  MOVE  04                 TO MEMO-PG
                  MOVE "#"                 TO MEMO-SEP
                  MOVE FIELD-NO            TO MEMO-FLD
                  MOVE OR-EDT-AMTYPE(SUB,S1) TO MEMO-OR-CHAR
                  MOVE CH-EDT-AMTYPE(SUB,S1) TO MEMO-CH-CHAR
                  PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *EDT-TERM - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 6
              IF SUB = 1
                  COMPUTE FIELD-NO = 10 + S1
              ELSE
                  COMPUTE FIELD-NO = 25 + S1
              END-IF
              IF OR-EDT-TERM(SUB,S1) NOT = CH-EDT-TERM(SUB,S1)
                  MOVE SPACES              TO MEMO-OR-CHAR
                  MOVE SPACES              TO MEMO-CH-CHAR
                  MOVE "TERM       "       TO MEMO-FIELD
                  MOVE  04                 TO MEMO-PG
                  MOVE "#"                 TO MEMO-SEP
                  MOVE FIELD-NO            TO MEMO-FLD
                  MOVE OR-EDT-TERM(SUB,S1) TO MEMO-OR-DEC1
                  MOVE CH-EDT-TERM(SUB,S1) TO MEMO-CH-DEC1
                  PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      * PAGE 5

      * SP-INSTBLE OCCURS 12 (ONLY 8 USED)
      *INSURANCE COMPANY NUMBER - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              IF OR-INSCOM(SUB) NOT = CH-INSCOM(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "INS CO#"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 01                  TO MEMO-FLD-NUM
                 MOVE OR-INSCOM(SUB)      TO MEMO-OR-NUM
                 MOVE CH-INSCOM(SUB)      TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *INSURANCE RATE CODE - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
             IF OR-INSRATECD(SUB) NOT = CH-INSRATECD(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "RATE CD"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 02                  TO MEMO-FLD-NUM
                 MOVE OR-INSRATECD(SUB)   TO MEMO-OR-NUM
                 MOVE CH-INSRATECD(SUB)   TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *INSURANCE DEVIATION - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
             IF OR-INSJFAC(SUB) NOT = CH-INSJFAC(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "DEVIATN"             TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 03                  TO MEMO-FLD-NUM
                 MOVE OR-INSJFAC(SUB)     TO MEMO-OR-DEC
                 MOVE CH-INSJFAC(SUB)     TO MEMO-CH-DEC
                 PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *INSURANCE DISCOUNT  - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
             IF OR-INSDISCNT(SUB) NOT = CH-INSDISCNT(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "DISCT  "           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 04                  TO MEMO-FLD-NUM
                 MOVE OR-INSDISCNT(SUB)   TO MEMO-OR-DEC
                 MOVE CH-INSDISCNT(SUB)   TO MEMO-CH-DEC
                 PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *INSURANCE CHARGEABLE- - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
             IF OR-INSCHGABL(SUB) NOT = CH-INSCHGABL(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "CHGABLE"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 07                  TO MEMO-FLD-NUM
                 MOVE OR-INSCHGABL(SUB)   TO MEMO-OR-CHAR
                 MOVE CH-INSCHGABL(SUB)   TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *INSURANCE RATE TABLE- - - - - - - - - - - - - - - - - - -
      * SP-INSRATETBL     OCCURS 20

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 20
             IF S1 = 1
                 MOVE 5 TO FIELD-NO
             ELSE
                 COMPUTE FIELD-NO = 27 + S1
             END-IF

             IF OR-INSTERM(SUB,S1) NOT = CH-INSTERM(SUB,S1)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 STRING "EXTBK",S1  DELIMITED BY "  " INTO
                                           REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO            TO MEMO-FLD-NUM
                 MOVE OR-INSTERM(SUB,S1)  TO MEMO-OR-NUM
                 MOVE CH-INSTERM(SUB,S1)  TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.


      *INSURANCE BREAKS - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 20
             IF S1 = 1
                 MOVE 6 TO FIELD-NO
             ELSE
                 COMPUTE FIELD-NO = 27 + S1
             END-IF
             IF OR-INSRATE(SUB,S1) NOT = CH-INSRATE(SUB,S1)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 STRING "EXTBK",S1  DELIMITED BY "  " INTO
                                           REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO            TO MEMO-FLD-NUM
                 MOVE OR-INSRATE(SUB,S1) TO MEMO-OR-DEC4
                 MOVE CH-INSRATE(SUB,S1) TO MEMO-CH-DEC4
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *MINIMUM INSURANCE PREMIUM- - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
             IF OR-INSMINPREM(SUB) NOT = CH-INSMINPREM(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "MIN PRM"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 09                  TO MEMO-FLD-NUM
                 MOVE OR-INSMINPREM(SUB)  TO MEMO-OR-DEC
                 MOVE CH-INSMINPREM(SUB)  TO MEMO-CH-DEC
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *MINIMUM CHARGEABLE- - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
             IF OR-INSMINCHGABL(SUB) NOT = CH-INSMINCHGABL(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "MIN CHG"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 08                  TO MEMO-FLD-NUM
                 MOVE OR-INSMINCHGABL(SUB) TO MEMO-OR-NUM
                 MOVE CH-INSMINCHGABL(SUB) TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *MAXIMUM CHARGEABLE- - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
             IF OR-INSMAXCHGABL(SUB) NOT = CH-INSMAXCHGABL(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "MAX CHG"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 08                  TO MEMO-FLD-NUM
                 MOVE OR-INSMAXCHGABL(SUB) TO MEMO-OR-NUM
                 MOVE CH-INSMAXCHGABL(SUB) TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *MAXIMUM BENEFIT- - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
                 IF OR-INSMAXBENIF(SUB) NOT = CH-INSMAXBENIF(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "MAX BEN"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 10                  TO MEMO-FLD-NUM
                 MOVE OR-INSMAXBENIF(SUB) TO MEMO-OR-CHAR
                 MOVE CH-INSMAXBENIF(SUB) TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *MAXIMUM TERM - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
             IF OR-INSMAXTERM(SUB) NOT = CH-INSMAXTERM(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "MAX TRM"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 11                  TO MEMO-FLD-NUM
                 MOVE OR-INSMAXTERM(SUB)  TO MEMO-OR-CHAR
                 MOVE CH-INSMAXTERM(SUB)  TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *CHARGEABLE OR BENEFIT- - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
             IF OR-INSCHGBEN(SUB) NOT = CH-INSCHGBEN(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "CHG/BEN"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 12                  TO MEMO-FLD-NUM
                 MOVE OR-INSCHGBEN(SUB)   TO MEMO-OR-CHAR
                 MOVE CH-INSCHGBEN(SUB)   TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *INSURANCE EXCEPTION - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
             IF OR-INSEXCEPT(SUB) NOT = CH-INSEXCEPT(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "EXCEPT "           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 14                  TO MEMO-FLD-NUM
                 MOVE OR-INSEXCEPT(SUB)   TO MEMO-OR-NUM
                 MOVE CH-INSEXCEPT(SUB)   TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *INTERNAL- - - - - - - - - - - - - - - - - - - - - - -
      *    PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
      *      IF OR-INSNOVER(SUB) NOT = CH-INSNOVER(SUB)
      *          MOVE SPACES              TO MEMO-OR-CHAR
      *          MOVE SPACES              TO MEMO-CH-CHAR
      *          MOVE SPACES              TO MEMO-FIELD
      *          PERFORM GET-INS-TYP-DESC
      *          MOVE "____________"      TO REST-OF-MEMO-INS
      *          MOVE "#"                 TO MEMO-SEP
      *          MOVE FIELD-NO            TO MEMO-FLD-NUM
      *          MOVE OR-INSNOVER(SUB)    TO MEMO-OR-NUM
      *          MOVE CH-INSNOVER(SUB)    TO MEMO-CH-NUM
      *          PERFORM CREATE-SM-MEMO
      *      END-IF
      *    END-PERFORM.

      *COVERAGE MIN & MAX AGE, AMOUNTS & TERM- - - - - - - -
      *SP-INSVERTBL           OCCURS 14
      *INSURANCE MIMIMUM AGE - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 14
             COMPUTE FIELD-NO = 14 + S1
             IF OR-INSMINAGE(SUB,S1) NOT = CH-INSMINAGE(SUB,S1)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "MIN AGE"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO            TO MEMO-FLD-NUM
                 MOVE OR-INSMINAGE(SUB,S1) TO MEMO-OR-NUM
                 MOVE CH-INSMINAGE(SUB,S1) TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *INSURANCE MAX AGE - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 14
             COMPUTE FIELD-NO = 14 + S1
             IF OR-INSMAXAGE(SUB,S1) NOT = CH-INSMAXAGE(SUB,S1)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "MAX AGE"            TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO            TO MEMO-FLD-NUM
                 MOVE OR-INSMAXAGE(SUB,S1) TO MEMO-OR-NUM
                 MOVE CH-INSMAXAGE(SUB,S1) TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *INSURANCE MAX COVERAGE- - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 14
             COMPUTE FIELD-NO = 14 + S1
             IF OR-INSMAXCOV(SUB,S1) NOT = CH-INSMAXCOV(SUB,S1)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "MAX COV"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO            TO MEMO-FLD-NUM
                 MOVE OR-INSMAXCOV(SUB,S1) TO MEMO-OR-NUM
                 MOVE CH-INSMAXCOV(SUB,S1) TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *INSURANCE MAX TERM - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 14
             COMPUTE FIELD-NO = 14 + S1
             IF OR-INSMAXTRM(SUB,S1) NOT = CH-INSMAXTRM(SUB,S1)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "MAX TRM"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO            TO MEMO-FLD-NUM
                 MOVE OR-INSMAXTRM(SUB,S1) TO MEMO-OR-NUM
                 MOVE CH-INSMAXTRM(SUB,S1) TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *INSURANCE FEE CODE - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
             IF OR-INSFEECD(SUB) NOT = CH-INSFEECD(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "FEE CD "           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 13                  TO MEMO-FLD-NUM
                 MOVE OR-INSFEECD(SUB)    TO MEMO-OR-CHAR
                 MOVE CH-INSFEECD(SUB)    TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *INSURANCE FEE FORMULA- - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
             IF OR-INSFEE-FRMLA(SUB) NOT = CH-INSFEE-FRMLA(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "FEE FRM"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 13                  TO MEMO-FLD-NUM
                 MOVE OR-INSFEE-FRMLA(SUB) TO MEMO-OR-CHAR
                 MOVE CH-INSFEE-FRMLA(SUB) TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *INSURANCE CHARGEABLE - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
             IF OR-INSCHGABL-FG(SUB) NOT = CH-INSCHGABL-FG(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "INS CHG"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 07                  TO MEMO-FLD-NUM
                 MOVE OR-INSCHGABL-FG(SUB) TO MEMO-OR-CHAR
                 MOVE CH-INSCHGABL-FG(SUB) TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *INSURANCE CHARGEABLE - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
             IF OR-INSREJECT-FG(SUB) NOT = CH-INSREJECT-FG(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "INS REJ"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 12                  TO MEMO-FLD-NUM
                 MOVE OR-INSREJECT-FG(SUB) TO MEMO-OR-CHAR
                 MOVE CH-INSREJECT-FG(SUB) TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *INSURANCE FINANCE CHARGE - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
             IF OR-INSFINCHG(SUB) NOT = CH-INSFINCHG(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-INS-TYP-DESC
                 MOVE "FIN CHG"           TO REST-OF-MEMO-INS
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 14                  TO MEMO-FLD-NUM
                 MOVE OR-INSFINCHG(SUB)   TO MEMO-OR-CHAR
                 MOVE CH-INSFINCHG(SUB)   TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *INSURANCE JOINT FLAG  - - - - - - - - - - - - - -
             IF OR-INSAH-JOINT-FG NOT = CH-INSAH-JOINT-FG
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE "JOINT FLAG"        TO MEMO-FIELD
                 MOVE 09                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE 14                  TO MEMO-FLD-NUM
                 MOVE OR-INSAH-JOINT-FG   TO MEMO-OR-CHAR
                 MOVE CH-INSAH-JOINT-FG   TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO.


      * PAGE 9

      *AH INSURANCE RETRO FLAG - - - - - - - - - - - - - - - - - - - - -
           IF OR-INSAH-RETRO NOT = CH-INSAH-RETRO
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "AH INS RETRO"      TO MEMO-FIELD
               MOVE "09#14"             TO MEMO-PG-FLD
               MOVE OR-INSAH-RETRO      TO MEMO-OR-CHAR
               MOVE CH-INSAH-RETRO      TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *AH RETRO DAYS - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF OR-INSAH-DAYS-WAIT NOT = CH-INSAH-DAYS-WAIT
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DAYS WAIT   "      TO MEMO-FIELD
               MOVE "09#14"             TO MEMO-PG-FLD
               MOVE OR-INSAH-DAYS-WAIT  TO MEMO-OR-NUM
               MOVE CH-INSAH-DAYS-WAIT  TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      * PAGE 12

      *REBATE TABLE - - - - - - - - - - - - - - - - - -
      * SP-REBTBL    OCCURS 16 (ONLY 14 USED)
      *REBATE FORMULA - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
             IF OR-RBFRMLA(SUB) NOT = CH-RBFRMLA(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-REBATE-TYP-DESC
                 MOVE "FRM CD "           TO REST-OF-MEMO-REB
                 MOVE 12                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBFRMLA(SUB)     TO MEMO-OR-NUM
                 MOVE CH-RBFRMLA(SUB)     TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *DEFAULT O2 INSURANCE COMPANY- - - - - - - - - - - - - - - - - -
           IF OR-DEFAULT-O2-INS-COMP NOT = CH-DEFAULT-O2-INS-COMP
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "OT2 INS CO  "      TO MEMO-FIELD
               MOVE "12#16"             TO MEMO-PG-FLD
               MOVE OR-DEFAULT-O2-INS-COMP TO MEMO-OR-CHAR
               MOVE CH-DEFAULT-O2-INS-COMP TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      * PAGE 13

      * SP-RBTBL OCCURS 2- - - - - - - - - - - - - - - - - -
      * SP-RBTBL                        OCCURS 2.
      * REBATE DATE - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 2
             IF OR-RBDTE(SUB,S1) NOT = CH-RBDTE(SUB,S1)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE SPACES              TO MEMO-FIELD
                 PERFORM GET-REBATE-TYP-DESC
                 STRING "REF" "(" S1 ")" DELIMITED BY "  "
                                          INTO REST-OF-MEMO-REB
                 MOVE 13                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBDTE(SUB,S1)    TO MEMO-OR-CHAR
                 MOVE CH-RBDTE(SUB,S1)    TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      * REBATE DAYS - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 2
             IF OR-RBDAYS(SUB,S1) NOT = CH-RBDAYS(SUB,S1)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE SPACES              TO MEMO-FIELD
                 PERFORM GET-REBATE-TYP-DESC
                 STRING "ADD" "(" S1 ")" DELIMITED BY "  "
                                          INTO REST-OF-MEMO-REB
                 MOVE 13                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBDAYS(SUB,S1)   TO MEMO-OR-NUM
                 MOVE CH-RBDAYS(SUB,S1)   TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      * REBATE YEAR TYPE- - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 2
             IF OR-RBYRTYPE(SUB,S1) NOT = CH-RBYRTYPE(SUB,S1)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE SPACES              TO MEMO-FIELD
                 PERFORM GET-REBATE-TYP-DESC
                 STRING "YR " "(" S1 ")" DELIMITED BY "  "
                                          INTO REST-OF-MEMO-REB
                 MOVE 13                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBYRTYPE(SUB,S1) TO MEMO-OR-NUM
                 MOVE CH-RBYRTYPE(SUB,S1) TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *MINIMUM RETENTION- - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
             IF OR-RBMINRET(SUB) NOT = CH-RBMINRET(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-REBATE-TYP-DESC
                 MOVE "RETENTN"           TO REST-OF-MEMO-REB
                 MOVE 12                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBMINRET(SUB)    TO MEMO-OR-DEC
                 MOVE CH-RBMINRET(SUB)    TO MEMO-CH-DEC
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *MINIMUM MONTHS- -- - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
             IF OR-RBNOMON(SUB) NOT = CH-RBNOMON(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-REBATE-TYP-DESC
                 MOVE "MIN MO "           TO REST-OF-MEMO-REB
                 MOVE 12                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBNOMON(SUB)     TO MEMO-OR-NUM
                 MOVE CH-RBNOMON(SUB)     TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *DEDUCT CHARGE - -- - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
             IF OR-RBDEDCHG(SUB) NOT = CH-RBDEDCHG(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-REBATE-TYP-DESC
                 MOVE "DEDUCT "           TO REST-OF-MEMO-REB
                 MOVE 12                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBDEDCHG(SUB)    TO MEMO-OR-DEC
                 MOVE CH-RBDEDCHG(SUB)    TO MEMO-CH-DEC
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *RECAST - - - - -- - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
             IF OR-RBRECAST(SUB) NOT = CH-RBRECAST(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-REBATE-TYP-DESC
                 MOVE "RECAST "           TO REST-OF-MEMO-REB
                 MOVE 13                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBRECAST(SUB)    TO MEMO-OR-NUM
                 MOVE CH-RBRECAST(SUB)    TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *EARLY PROVISION-- - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
             IF OR-RBEARLY(SUB) NOT = CH-RBEARLY(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-REBATE-TYP-DESC
                 MOVE "E PROV "           TO REST-OF-MEMO-REB
                 MOVE 12                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBEARLY(SUB)     TO MEMO-OR-NUM
                 MOVE CH-RBEARLY(SUB)     TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *REBATE DEFERMENT- - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
             IF OR-RBDEFER(SUB) NOT = CH-RBDEFER(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-REBATE-TYP-DESC
                 MOVE "DEF CD "           TO REST-OF-MEMO-REB
                 MOVE 12                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBDEFER(SUB) TO MEMO-OR-NUM
                 MOVE CH-RBDEFER(SUB) TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *REFUND PERCENTAGE - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
             IF OR-RBPCT(SUB) NOT = CH-RBPCT(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-REBATE-TYP-DESC
                 MOVE "REF PCT"           TO REST-OF-MEMO-REB
                 MOVE 12                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBPCT(SUB)       TO MEMO-OR-DEC1
                 MOVE CH-RBPCT(SUB)       TO MEMO-CH-DEC1
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *REDUCTION AMOUNT  - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
             IF OR-RBREDUC(SUB) NOT = CH-RBREDUC(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-REBATE-TYP-DESC
                 MOVE "REDUCT "           TO REST-OF-MEMO-REB
                 MOVE 12                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBREDUC(SUB)     TO MEMO-OR-DEC
                 MOVE CH-RBREDUC(SUB)     TO MEMO-CH-DEC
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *REBATE MINIMUM   - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
             IF OR-RBMIN(SUB) NOT = CH-RBMIN(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-REBATE-TYP-DESC
                 MOVE "REB MIN"           TO REST-OF-MEMO-REB
                 MOVE 12                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBMIN(SUB)       TO MEMO-OR-DEC
                 MOVE CH-RBMIN(SUB)       TO MEMO-CH-DEC
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *REBATE SPECIAL OPTION 1- - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
             IF OR-RBSPOPT1(SUB) NOT = CH-RBSPOPT1(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-REBATE-TYP-DESC
                 MOVE "SP OPT1"           TO REST-OF-MEMO-REB
                 MOVE 12                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBSPOPT1(SUB)    TO MEMO-OR-NUM
                 MOVE CH-RBSPOPT1(SUB)    TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *REBATE SPECIAL OPTION 2- - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
             IF OR-RBSPOPT2(SUB) NOT = CH-RBSPOPT2(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-REBATE-TYP-DESC
                 MOVE "SP OPT2"           TO REST-OF-MEMO-REB
                 MOVE 13                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBSPOPT2(SUB)    TO MEMO-OR-NUM
                 MOVE CH-RBSPOPT2(SUB)    TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *MANAGER OPTION (NOT USED)- - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
             IF OR-RBMANAGER(SUB) NOT = CH-RBMANAGER(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-REBATE-TYP-DESC
                 MOVE "MGR OPT"           TO REST-OF-MEMO-REB
                 MOVE 13                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBMANAGER(SUB)   TO MEMO-OR-CHAR
                 MOVE CH-RBMANAGER(SUB)   TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *PAY FREQUENCY - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
             IF OR-RBUNITPER-CD(SUB) NOT = CH-RBUNITPER-CD(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-REBATE-TYP-DESC
                 MOVE "PAY FRQ"           TO REST-OF-MEMO-REB
                 MOVE 13                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBUNITPER-CD(SUB) TO MEMO-OR-CHAR
                 MOVE CH-RBUNITPER-CD(SUB) TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *REBATE FORMULA 2- - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 14
             IF OR-RBFRMLA2(SUB) NOT = CH-RBFRMLA2(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 PERFORM GET-REBATE-TYP-DESC
                 MOVE "FRMLA 2"           TO REST-OF-MEMO-REB
                 MOVE 12                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE SUB                 TO MEMO-FLD-NUM
                 MOVE OR-RBFRMLA2(SUB)    TO MEMO-OR-NUM
                 MOVE CH-RBFRMLA2(SUB)    TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *COMBINED - - - - - - - - - - - - - - - - - - - - -
           IF OR-RBCOMCD NOT = CH-RBCOMCD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "COMBINED   "       TO MEMO-FIELD
               MOVE "12#15"             TO MEMO-PG-FLD
               MOVE OR-RBCOMCD          TO MEMO-OR-CHAR
               MOVE CH-RBCOMCD          TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.



      *ROUND PAY FLAG - - - - - - - - - - - - - - - - - - - -
             IF OR-ROUND-PAY-FG NOT = CH-ROUND-PAY-FG
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "ROUND PY FG"       TO MEMO-FIELD
               MOVE "     "             TO MEMO-PG-FLD
               MOVE OR-ROUND-PAY-FG     TO MEMO-OR-CHAR
               MOVE CH-ROUND-PAY-FG     TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.


      *MAXIUM ALLOWABLE DELINQUENCIES- - - - - - - - - - - -
           IF OR-MAX-ALLOW-DELINQ NOT = CH-MAX-ALLOW-DELINQ
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "MAX AD'S   "       TO MEMO-FIELD
               MOVE "02#13"             TO MEMO-PG-FLD
               MOVE OR-MAX-ALLOW-DELINQ TO MEMO-OR-NUM
               MOVE CH-MAX-ALLOW-DELINQ TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *RECISSION DAYS- - - - - - - - - - - - - - - - - - - -
           IF OR-RESCIND-BUS-DAYS NOT = CH-RESCIND-BUS-DAYS
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "RESCIND DAY"       TO MEMO-FIELD
               MOVE "01#13"             TO MEMO-PG-FLD
               MOVE OR-RESCIND-BUS-DAYS TO MEMO-OR-CHAR
               MOVE CH-RESCIND-BUS-DAYS TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *RECISSION INCLUDE SATURDAY- - - - - - - - - - - - - -
           IF OR-RESCIND-SATURDAY NOT = CH-RESCIND-SATURDAY
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "RESCIND SAT"       TO MEMO-FIELD
               MOVE "01#14"             TO MEMO-PG-FLD
               MOVE OR-RESCIND-SATURDAY TO MEMO-OR-CHAR
               MOVE CH-RESCIND-SATURDAY TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *CAN LOAN FLAG - - - - - - - - - - - - - - - - - - - -
             IF OR-CAN-LOANS-FG NOT = CH-CAN-LOANS-FG
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CAN LOAN   "       TO MEMO-FIELD
               MOVE "01#15"             TO MEMO-PG-FLD
               MOVE OR-CAN-LOANS-FG TO MEMO-OR-CHAR
               MOVE CH-CAN-LOANS-FG TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.



      *ALLOWABLE DELINQ (AD) POLICY CODE - - - - - - - - - -
             IF OR-ALLOW-DELINQ-POLCD NOT = CH-ALLOW-DELINQ-POLCD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "AD POLICY  "       TO MEMO-FIELD
               MOVE "02#11"             TO MEMO-PG-FLD
               MOVE OR-ALLOW-DELINQ-POLCD TO MEMO-OR-CHAR
               MOVE CH-ALLOW-DELINQ-POLCD TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *YTD MAXIMUM ALLOWABLE DELINQUENCIES - - - - - - - - -
             IF OR-YTD-MAX-ALLOW-DELINQ NOT = CH-YTD-MAX-ALLOW-DELINQ
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "YTD MAX AD "       TO MEMO-FIELD
               MOVE "02#12"             TO MEMO-PG-FLD
               MOVE OR-YTD-MAX-ALLOW-DELINQ TO MEMO-OR-NUM
               MOVE CH-YTD-MAX-ALLOW-DELINQ TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      * PAGE 14

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *EARNINGS TABLE - (INTEREST EARNINGS) - - - - - - - - - - - -
      *    SP-ERNPERTBL                   OCCURS 6
      *EARNINGS FORMULA- - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SUB = 2 OR 4 OR 6
                MOVE 1  TO FIELD-NO
             ELSE
                MOVE 16 TO FIELD-NO
             END-IF
               IF OR-ERNFRMLA(SUB) NOT =  CH-ERNFRMLA(SUB)
                   MOVE SPACES              TO MEMO-OR-CHAR
                   MOVE SPACES              TO MEMO-CH-CHAR
                   MOVE SPACES              TO MEMO-FIELD
                   PERFORM GET-EARN-TYP-DESC
                   MOVE "ERN FRML"          TO REST-OF-MEMO-EARN
                   MOVE "#"                 TO MEMO-SEP
                   MOVE FIELD-NO            TO MEMO-FLD-NUM
                   MOVE OR-ERNFRMLA(SUB)    TO MEMO-OR-NUM
                   MOVE CH-ERNFRMLA(SUB)    TO MEMO-CH-NUM
                   PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.

      *UPFRONT EARNINGS AMOUNT - - - - - - - - - - - - - - - - - -
      *SP-ERNAMT
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SUB = 2 OR 4 OR 6
                MOVE 2   TO FIELD-NO
             ELSE
                MOVE 17  TO FIELD-NO
             END-IF
               IF OR-ERNAMT(SUB) NOT =  CH-ERNAMT(SUB)
                   MOVE SPACES              TO MEMO-OR-CHAR
                   MOVE SPACES              TO MEMO-CH-CHAR
                   MOVE SPACES              TO MEMO-FIELD
                   PERFORM GET-EARN-TYP-DESC
                   MOVE "UPFR AMT"          TO REST-OF-MEMO-EARN
                   MOVE "#"                 TO MEMO-SEP
                   MOVE FIELD-NO            TO MEMO-FLD-NUM
                   MOVE OR-ERNAMT(SUB)      TO MEMO-OR-DEC
                   MOVE CH-ERNAMT(SUB)      TO MEMO-CH-DEC
                   PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.

      *UPFRONT EARNINGS INTEREST PERCENT CODE- - - - - - - - - - -
      *SP-ERNPERCT
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SUB = 2 OR 4 OR 6
                MOVE 3 TO FIELD-NO
             ELSE
                MOVE 18 TO FIELD-NO
             END-IF
               IF OR-ERNPERCT(SUB) NOT =  CH-ERNPERCT(SUB)
                   MOVE SPACES              TO MEMO-OR-CHAR
                   MOVE SPACES              TO MEMO-CH-CHAR
                   MOVE SPACES              TO MEMO-FIELD
                   PERFORM GET-EARN-TYP-DESC
                   MOVE "UPFR %CD"          TO REST-OF-MEMO-EARN
                   MOVE "#"                 TO MEMO-SEP
                   MOVE FIELD-NO            TO MEMO-FLD-NUM
                   MOVE OR-ERNPERCT(SUB)    TO MEMO-OR-DEC
                   MOVE CH-ERNPERCT(SUB)    TO MEMO-CH-DEC
                   PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.

      *UPFRONT EARNINGS PERCENT- - - - - - - - - - - - - - - - - -
      *SP-ERNPERCD
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SUB = 2 OR 4 OR 6
                MOVE 3   TO FIELD-NO
             ELSE
                MOVE 18  TO FIELD-NO
             END-IF
               IF OR-ERNPERCD(SUB) NOT =  CH-ERNPERCD(SUB)
                   MOVE SPACES              TO MEMO-OR-CHAR
                   MOVE SPACES              TO MEMO-CH-CHAR
                   MOVE SPACES              TO MEMO-FIELD
                   PERFORM GET-EARN-TYP-DESC
                   MOVE "UPFR %  "          TO REST-OF-MEMO-EARN
                   MOVE "#"                 TO MEMO-SEP
                   MOVE FIELD-NO            TO MEMO-FLD-NUM
                   MOVE OR-ERNPERCD(SUB)    TO MEMO-OR-DEC
                   MOVE CH-ERNPERCD(SUB)    TO MEMO-CH-DEC
                   PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.

      *ACCRUAL  CODE (WHEN TO TAKE 1ST SLICE OF EARNINGS) -
      *SP-ERNACCRL-FG
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SUB = 2 OR 4 OR 6
                MOVE 4   TO FIELD-NO
             ELSE
                MOVE 19  TO FIELD-NO
             END-IF
               IF OR-ERNACCRL-FG(SUB) NOT =  CH-ERNACCRL-FG(SUB)
                   MOVE SPACES              TO MEMO-OR-CHAR
                   MOVE SPACES              TO MEMO-CH-CHAR
                   MOVE SPACES              TO MEMO-FIELD
                   PERFORM GET-EARN-TYP-DESC
                   MOVE "ACCL ERN"          TO REST-OF-MEMO-EARN
                   MOVE "#"                 TO MEMO-SEP
                   MOVE FIELD-NO            TO MEMO-FLD-NUM
                   MOVE OR-ERNACCRL-FG(SUB) TO MEMO-OR-DEC
                   MOVE CH-ERNACCRL-FG(SUB) TO MEMO-CH-DEC
                   PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.

      *START EARNINGS CODE- - - - - - - - - - - - - - - - - - - - -
      * SP-ERNACCRL-START-CD
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SUB = 2 OR 4 OR 6
                MOVE 5   TO FIELD-NO
             ELSE
                MOVE 20  TO FIELD-NO
             END-IF
               IF OR-ERNACCRL-START-CD(SUB) NOT =
                                     CH-ERNACCRL-START-CD(SUB)
                   MOVE SPACES              TO MEMO-OR-CHAR
                   MOVE SPACES              TO MEMO-CH-CHAR
                   MOVE SPACES              TO MEMO-FIELD
                   PERFORM GET-EARN-TYP-DESC
                   MOVE "START CD"          TO REST-OF-MEMO-EARN
                   MOVE "#"                 TO MEMO-SEP
                   MOVE FIELD-NO            TO MEMO-FLD-NUM
                   MOVE OR-ERNACCRL-START-CD(SUB) TO MEMO-OR-CHAR
                   MOVE CH-ERNACCRL-START-CD(SUB) TO MEMO-CH-CHAR
                   PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.

      *EFFECT DEFERMENTS HAVE ON ACCRUALS - - - - - - - - - - - - -
      *SP-ERNACCRL-DFCD
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SUB = 2 OR 4 OR 6
                MOVE 6   TO FIELD-NO
             ELSE
                MOVE 21  TO FIELD-NO
             END-IF
               IF OR-ERNACCRL-DFCD(SUB) NOT =  CH-ERNACCRL-DFCD(SUB)
                   MOVE SPACES              TO MEMO-OR-CHAR
                   MOVE SPACES              TO MEMO-CH-CHAR
                   MOVE SPACES              TO MEMO-FIELD
                   PERFORM GET-EARN-TYP-DESC
                   MOVE "ACC DFCD"          TO REST-OF-MEMO-EARN
                   MOVE "#"                 TO MEMO-SEP
                   MOVE FIELD-NO            TO MEMO-FLD-NUM
                   MOVE OR-ERNACCRL-DFCD(SUB) TO MEMO-OR-CHAR
                   MOVE CH-ERNACCRL-DFCD(SUB) TO MEMO-CH-CHAR
                   PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.

      *ELAPSED DAYS TO TAKE EARNINGS- - - - - - - - - - - - - - - -
      *SP-ERN-DAYS
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SUB = 2 OR 4 OR 6
                MOVE 7   TO FIELD-NO
             ELSE
                MOVE 22  TO FIELD-NO
             END-IF
               IF OR-ERN-DAYS(SUB) NOT =  CH-ERN-DAYS(SUB)
                   MOVE SPACES              TO MEMO-OR-CHAR
                   MOVE SPACES              TO MEMO-CH-CHAR
                   MOVE SPACES              TO MEMO-FIELD
                   PERFORM GET-EARN-TYP-DESC
                   MOVE "EARN DAY"          TO REST-OF-MEMO-EARN
                   MOVE "#"                 TO MEMO-SEP
                   MOVE FIELD-NO            TO MEMO-FLD-NUM
                   MOVE OR-ERN-DAYS(SUB)    TO MEMO-OR-NUM
                   MOVE CH-ERN-DAYS(SUB)    TO MEMO-CH-NUM
                   PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.

      *FIXED TERM-(NUMBER OF MONTHS TO SPREAD EARNINGS - - - - - -
      *SP-ERN-TERM
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SUB = 2 OR 4 OR 6
                MOVE 8   TO FIELD-NO
             ELSE
                MOVE 23  TO FIELD-NO
             END-IF
               IF OR-ERN-TERM(SUB) NOT =  CH-ERN-TERM(SUB)
                   MOVE SPACES              TO MEMO-OR-CHAR
                   MOVE SPACES              TO MEMO-CH-CHAR
                   MOVE SPACES              TO MEMO-FIELD
                   PERFORM GET-EARN-TYP-DESC
                   MOVE "ACCL ERN"          TO REST-OF-MEMO-EARN
                   MOVE "#"                 TO MEMO-SEP
                   MOVE FIELD-NO            TO MEMO-FLD-NUM
                   MOVE OR-ERN-TERM(SUB)    TO MEMO-OR-NUM
                   MOVE CH-ERN-TERM(SUB)    TO MEMO-CH-NUM
                   PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.
      *ACCELERATED EARNING CODE - - - - - - - - - - - - - - - - - -
           IF OR-ERNACCEL-CD NOT = OR-ERNACCEL-CD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "ACCEL EARN "       TO MEMO-FIELD
               MOVE "14#01"             TO MEMO-PG-FLD
               MOVE OR-ERNACCEL-CD      TO MEMO-OR-CHAR
               MOVE CH-ERNACCEL-CD      TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *JUDGEMENT EARNING CODE - - - - - - - - - - - - - - - - - - -
           IF OR-ERNJD-CD NOT = OR-ERNJD-CD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "JDG EARN   "       TO MEMO-FIELD
               MOVE "14#02"             TO MEMO-PG-FLD
               MOVE OR-ERNJD-CD         TO MEMO-OR-CHAR
               MOVE CH-ERNJD-CD         TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *MIDSTREAM EARNINGS CODE- - - - - - - - - - - - - - - - - - -
           IF OR-MIDSTRERNCD NOT = OR-MIDSTRERNCD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "MIDSTR EARN"       TO MEMO-FIELD
               MOVE "14#03"             TO MEMO-PG-FLD
               MOVE OR-MIDSTRERNCD      TO MEMO-OR-CHAR
               MOVE CH-MIDSTRERNCD      TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *DELINQUENCY STOP FORMULA - - - - - - - - - - - - - - - - - -
           IF OR-ERNDELQFRMLA NOT = CH-ERNDELQFRMLA
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DEL STOP FR"       TO MEMO-FIELD
               MOVE "14#04"             TO MEMO-PG-FLD
               MOVE OR-ERNDELQFRMLA     TO MEMO-OR-CHAR
               MOVE CH-ERNDELQFRMLA     TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *DELINQUENCY STOP EARNINGS - - - - - - - - - - - - - - - - -
           IF OR-ERNDELQ-STOP NOT = CH-ERNDELQ-STOP
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DELINQ STOP"       TO MEMO-FIELD
               MOVE "14#05"             TO MEMO-PG-FLD
               MOVE OR-ERNDELQ-STOP     TO MEMO-OR-NUM
               MOVE CH-ERNDELQ-STOP     TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *DELINQUENCY START EARNINGS - - - - - - - - - - - - - - - - -
           IF OR-ERNDELQ-START NOT = CH-ERNDELQ-START
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DELQ START  "      TO MEMO-FIELD
               MOVE "14#06"             TO MEMO-PG-FLD
               MOVE OR-ERNDELQ-START    TO MEMO-OR-NUM
               MOVE CH-ERNDELQ-START    TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.


      * 14#07 IS UNUSED

      *INTEREST BEARING MAX ACCRUE DAYS - - - - - - - - - - - - - -
           IF OR-MAX-IB-ACR-DAYS NOT = CH-MAX-IB-ACR-DAYS
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "IB MAX DAYS"       TO MEMO-FIELD
               MOVE "14#08"             TO MEMO-PG-FLD
               MOVE OR-MAX-IB-ACR-DAYS  TO MEMO-OR-NUM
               MOVE CH-MAX-IB-ACR-DAYS  TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *IB INTEREST DELIQUENCY + AGE- - - - - - - - - - - - - - - - - -
           IF OR-DELQ-IB-AGE NOT = CH-DELQ-IB-AGE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DELQ+AGE DAY"      TO MEMO-FIELD
               MOVE "14#09"             TO MEMO-PG-FLD
               MOVE OR-DELQ-IB-AGE      TO MEMO-OR-NUM
               MOVE CH-DELQ-IB-AGE      TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *-IB DELQ-IB-MAX-DAYS- - - - - - - - - - - - - - - - - - - - - -
           IF OR-DELQ-IB-MAX-DAYS NOT = CH-DELQ-IB-MAX-DAYS
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DELQ+AGE MAX"      TO MEMO-FIELD
               MOVE "14#09"             TO MEMO-PG-FLD
               MOVE OR-DELQ-IB-MAX-DAYS TO MEMO-OR-NUM
               MOVE CH-DELQ-IB-MAX-DAYS TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *- - - - - -  - - - - - - - - - - - - - - - - - - - - - - - -
      *SP-EARN-ORDER           OCCURS 6
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               COMPUTE FIELD-NO = 9 + SUB
               IF OR-EARN-ORDER(SUB) NOT =  CH-EARN-ORDER(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE SPACES              TO MEMO-FIELD
                 STRING "EARN ORD   " "(" SUB ")" DELIMITED BY
                    "  " INTO  MEMO-FIELD
                 MOVE  14                 TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO                TO MEMO-FLD-NUM
                 MOVE OR-EARN-ORDER(SUB)  TO MEMO-OR-NUM
                 MOVE CH-EARN-ORDER(SUB)  TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.

      * PAGE 16

      *DEALER DISCOUNT RESERVE PRINCIPAL 1 PERCENT - - - - - - - - - - -
           IF OR-DLDISC-RVPRIN1-PCNT NOT = CH-DLDISC-RVPRIN1-PCNT
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DLDISC RVP1%"      TO MEMO-FIELD
               MOVE "16#24"             TO MEMO-PG-FLD
               MOVE OR-DLDISC-RVPRIN1-PCNT TO MEMO-OR-DEC
               MOVE CH-DLDISC-RVPRIN1-PCNT TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *DEALER DISCOUNT RESERVE PRINCIPAL 1 AF PERCENT- - - - - - - - - -
           IF OR-DLDISC-RVPRIN1-AF-PCNTX
                                  NOT = CH-DLDISC-RVPRIN1-AF-PCNTX
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "RVPRIN1 AF %"      TO MEMO-FIELD
               MOVE "16#25"             TO MEMO-PG-FLD
               MOVE OR-DLDISC-RVPRIN1-AF-PCNTX TO MEMO-OR-DEC
               MOVE CH-DLDISC-RVPRIN1-AF-PCNTX TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

           IF OR-DLDISC-CONV-BF-PCNT NOT = CH-DLDISC-CONV-BF-PCNT
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DLDISC BF % "      TO MEMO-FIELD
               MOVE "16#26"             TO MEMO-PG-FLD
               MOVE OR-DLDISC-CONV-BF-PCNT TO MEMO-OR-DEC3
               MOVE CH-DLDISC-CONV-BF-PCNT TO MEMO-CH-DEC3
               PERFORM CREATE-SM-MEMO.

      * PAGE 17

      *LOAN COST FORMULA - - - - - - - - - - - - - - - - - - - - - - - -
           IF OR-LNCOST-FRMLA NOT = CH-LNCOST-FRMLA
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "LOAN CST FR"       TO MEMO-FIELD
               MOVE "17#11"             TO MEMO-PG-FLD
               MOVE OR-LNCOST-FRMLA TO MEMO-OR-CHAR
               MOVE CH-LNCOST-FRMLA TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *LOAN COST PERCENT - - - - - - - - - - - - - - - - - - - - - - - -
           IF OR-LNCOST-AMT NOT = CH-LNCOST-AMT
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "LOAN CST % "       TO MEMO-FIELD
               MOVE "17#12"             TO MEMO-PG-FLD
               MOVE OR-LNCOST-AMT       TO MEMO-OR-DEC
               MOVE CH-LNCOST-AMT       TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *DUAL LOANS IN STATE - - - - - - - - - - - - - - - - -
             IF OR-DUAL-IN-STATE NOT = CH-DUAL-IN-STATE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DUAL LN ST "       TO MEMO-FIELD
               MOVE "17#14"             TO MEMO-PG-FLD
               MOVE OR-DUAL-IN-STATE    TO MEMO-OR-CHAR
               MOVE CH-DUAL-IN-STATE    TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *DUAL LOANS IN BRANCH- - - - - - - - - - - - - - - - -
             IF OR-DUAL-IN-BRANCH NOT = CH-DUAL-IN-BRANCH
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "DUAL LN BR "       TO MEMO-FIELD
               MOVE "17#15"             TO MEMO-PG-FLD
               MOVE OR-DUAL-IN-BRANCH   TO MEMO-OR-CHAR
               MOVE CH-DUAL-IN-BRANCH   TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *1ST PAY ADD ON DAYS - - - - - - - - - - -  - - - -
           IF OR-1STPY-ADDON-DAYS NOT = CH-1STPY-ADDON-DAYS
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CONS PY SCH"       TO MEMO-FIELD
               MOVE "17#16"             TO MEMO-PG-FLD
               MOVE OR-1STPY-ADDON-DAYS TO MEMO-OR-NUM
               MOVE CH-1STPY-ADDON-DAYS TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *ADDON YEAR TYPE -  - - - - - - - - - - -  - - - -
           IF OR-1STPY-ADDON-YRTYPE NOT = CH-1STPY-ADDON-YRTYPE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "ADDON YR TY"       TO MEMO-FIELD
               MOVE "17#17"             TO MEMO-PG-FLD
               MOVE OR-1STPY-ADDON-YRTYPE TO MEMO-OR-NUM
               MOVE CH-1STPY-ADDON-YRTYPE TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *ADDON RATE TYPE -  - - - - - - - - - - -  - - - -
           IF OR-ADDON-RATE-FG NOT = CH-ADDON-RATE-FG
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "ADDON RT TY"       TO MEMO-FIELD
               MOVE "17#18"             TO MEMO-PG-FLD
               MOVE OR-ADDON-RATE-FG    TO MEMO-OR-CHAR
               MOVE CH-ADDON-RATE-FG    TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *ADDON RATE - - -  - - - - - - - - - - -  - - - -
           IF OR-ADDON-RATE NOT = CH-ADDON-RATE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "ADDON RATE "       TO MEMO-FIELD
               MOVE "17#19"             TO MEMO-PG-FLD
               MOVE OR-ADDON-RATE       TO MEMO-OR-DEC3
               MOVE CH-ADDON-RATE       TO MEMO-CH-DEC3
               PERFORM CREATE-SM-MEMO.

      *CONSOLIDATE PAY SCHEDULE TO EVEN PAYMENTS- - - - -
           IF OR-CONSOLIDATE-RT-FG NOT = CH-CONSOLIDATE-RT-FG
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CONS PY SCH"       TO MEMO-FIELD
               MOVE "17#20"             TO MEMO-PG-FLD
               MOVE OR-CONSOLIDATE-RT-FG TO MEMO-OR-CHAR
               MOVE CH-CONSOLIDATE-RT-FG TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *ORIGINAL OR UPDATED MATURITY - - - - - - - - - - -
           IF OR-ADDON-ORG-MATURITY NOT = CH-ADDON-ORG-MATURITY
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "ORIG/UP MAT"       TO MEMO-FIELD
               MOVE "17#21"             TO MEMO-PG-FLD
               MOVE OR-ADDON-ORG-MATURITY TO MEMO-OR-CHAR
               MOVE CH-ADDON-ORG-MATURITY TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *ROUND TO WHOLE DOLLARS FLAG- - - - - - - - - - - -
           IF OR-ADDON-WHOLE-FG NOT = CH-ADDON-WHOLE-FG
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "ROUND TO $ "       TO MEMO-FIELD
               MOVE "17#22"             TO MEMO-PG-FLD
               MOVE OR-ADDON-WHOLE-FG   TO MEMO-OR-CHAR
               MOVE CH-ADDON-WHOLE-FG   TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *ADDON DISCLOSURE - - - - - - - - - - - - - - - - -
           IF OR-ADDON-DISFRMLA NOT = CH-ADDON-DISFRMLA
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "ADDON DISC "       TO MEMO-FIELD
               MOVE "17#23"             TO MEMO-PG-FLD
               MOVE OR-ADDON-DISFRMLA   TO MEMO-OR-CHAR
               MOVE CH-ADDON-DISFRMLA   TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *ADDON EXCEPTION FLAG- - - - - - - - - - - - - - - - - - - - - - -
           IF OR-ADDON-EXCEPT-FG NOT = CH-ADDON-EXCEPT-FG
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "ADDON EXCEPT"      TO MEMO-FIELD
               MOVE "17#24"             TO MEMO-PG-FLD
               MOVE OR-ADDON-EXCEPT-FG TO MEMO-OR-CHAR
               MOVE CH-ADDON-EXCEPT-FG TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *OPTIONS WHEN CANCELING ADDONS - - - - - - - - - - - - - - - - -
             IF OR-CANCEL-ADDON-FG NOT = CH-CANCEL-ADDON-FG
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CANCEL ADDON"      TO MEMO-FIELD
               MOVE "17#25"             TO MEMO-PG-FLD
               MOVE OR-CANCEL-ADDON-FG  TO MEMO-OR-CHAR
               MOVE CH-CANCEL-ADDON-FG  TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *INSURANCE O1 EDIT - - - - - - - - - - - - - - - -
             IF OR-INS-O1-EDIT NOT = CH-INS-O1-EDIT
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE "O1 EDIT   "        TO MEMO-FIELD
                 MOVE "17#29"             TO MEMO-PG-FLD
                 MOVE OR-INS-O1-EDIT      TO MEMO-OR-CHAR
                 MOVE CH-INS-O1-EDIT      TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO.

      *INSURANCE O2 EDIT - - - - - - - - - - - - - - - -
             IF OR-INS-O2-EDIT NOT = CH-INS-O2-EDIT
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE "O2 EDIT   "        TO MEMO-FIELD
                 MOVE "17#30"             TO MEMO-PG-FLD
                 MOVE OR-INS-O2-EDIT      TO MEMO-OR-CHAR
                 MOVE CH-INS-O2-EDIT      TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO.

      * PAGE 18

      *IRREGULAR PAY FLAG - - - - - - - - - - - - - - - - - -
           IF OR-IRREGULAR-PAY-FG NOT = CH-IRREGULAR-PAY-FG
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "IRREG PY FG"       TO MEMO-FIELD
               MOVE "18#01"             TO MEMO-PG-FLD
               MOVE OR-IRREGULAR-PAY-FG TO MEMO-OR-CHAR
               MOVE CH-IRREGULAR-PAY-FG TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      * RESERVE CHARGES RECAPTURE - - - - - - - - - - - - - -
      * SP-DLRVCHGS-RB-CD    OCCURS 3
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
             IF OR-DLRVCHGS-RB-CD(SUB) NOT = CH-DLRVCHGS-RB-CD(SUB)
                 COMPUTE FIELD-NO =  1 + SUB
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE SPACES              TO MEMO-FIELD
                 STRING "RE-CAPT" "  " "(" SUB ")"
                         DELIMITED BY "  " INTO MEMO-FIELD
                 MOVE 18                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO            TO MEMO-FLD-NUM
                 MOVE OR-DLRVCHGS-RB-CD(SUB)
                                          TO MEMO-OR-CHAR
                 MOVE CH-DLRVCHGS-RB-CD(SUB)
                                          TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      * PARTICIPATING HELD RESERVE RECAPTURE- - - - - - - - -
           IF OR-PARVHELD-RB-CD NOT = CH-PARVHELD-RB-CD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "HELD RECAPT"       TO MEMO-FIELD
               MOVE "18#05"             TO MEMO-PG-FLD
               MOVE OR-PARVHELD-RB-CD   TO MEMO-OR-CHAR
               MOVE CH-PARVHELD-RB-CD   TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *PARTICIPATING PAID RESERVE RECAPTURE CD-  - - - - - -
           IF OR-PARVPAID-RB-CD NOT = CH-PARVPAID-RB-CD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "PAID RECAPT"       TO MEMO-FIELD
               MOVE "18#06"             TO MEMO-PG-FLD
               MOVE OR-PARVPAID-RB-CD   TO MEMO-OR-CHAR
               MOVE CH-PARVPAID-RB-CD   TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *CONTRACTUAL CREDIT- - - - - - - - - - - - - - - - - -
           IF OR-CONTR-CREDIT NOT = CH-CONTR-CREDIT
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CONT CREDIT"       TO MEMO-FIELD
               MOVE "18#07"             TO MEMO-PG-FLD
               MOVE OR-CONTR-CREDIT     TO MEMO-OR-DEC
               MOVE CH-CONTR-CREDIT     TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *CONTRACTUAL CREDIT CODE - - - - - - - - - - - - - - -
           IF OR-CONTR-CREDIT-CD NOT = CH-CONTR-CREDIT-CD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CON CRD CD "       TO MEMO-FIELD
               MOVE "18#07"             TO MEMO-PG-FLD
               MOVE OR-CONTR-CREDIT-CD  TO MEMO-OR-CHAR
               MOVE CH-CONTR-CREDIT-CD  TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *CONTRACTUAL CREDIT MIN DELINQUENCY PERCENT- - - - - -
           IF OR-CONTR-CREDIT-MIN-DEL-PER
                             NOT = CH-CONTR-CREDIT-MIN-DEL-PER
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CREDIT DEL%"       TO MEMO-FIELD
               MOVE "18#07"             TO MEMO-PG-FLD
               MOVE OR-CONTR-CREDIT-MIN-DEL-PER TO MEMO-OR-DEC1
               MOVE CH-CONTR-CREDIT-MIN-DEL-PER TO MEMO-CH-DEC1
               PERFORM CREATE-SM-MEMO.

      *OPTIONAL CHARGEABLE CODE-(USED AT POST TIME) - - - - -
           IF OR-OPT-CAL-CHGABLE NOT = CH-OPT-CAL-CHGABLE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "OPT CAL CHG"       TO MEMO-FIELD
               MOVE "18#08"             TO MEMO-PG-FLD
               MOVE OR-OPT-CAL-CHGABLE  TO MEMO-OR-CHAR
               MOVE CH-OPT-CAL-CHGABLE  TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *PB DELQ DAYS- - - - - - - - - - - - - - - - - - - - -
           IF OR-PBDEL-DAYS NOT = CH-PBDEL-DAYS
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "PD DELQ DAY"       TO MEMO-FIELD
               MOVE "18#09"             TO MEMO-PG-FLD
               MOVE OR-PBDEL-DAYS       TO MEMO-OR-NUM
               MOVE CH-PBDEL-DAYS       TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *PB INCREASE PERCENT - - - - - - - - - - - - - - - - -
           IF OR-PBINCR-PERCENT NOT = CH-PBINCR-PERCENT
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "HELD RECAPT"       TO MEMO-FIELD
               MOVE "18#10"             TO MEMO-PG-FLD
               MOVE OR-PBINCR-PERCENT   TO MEMO-OR-NUM
               MOVE CH-PBINCR-PERCENT   TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *POTENTIAL, MISPAY(CONTRACTUAL)- - - - - - - - - - - -
           IF OR-CPOT30-CD NOT = CH-CPOT30-CD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CON POTENTL"       TO MEMO-FIELD
               MOVE "18#11"             TO MEMO-PG-FLD
               MOVE OR-CPOT30-CD        TO MEMO-OR-CHAR
               MOVE CH-CPOT30-CD        TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *CHARGE OFF CODE CODE-- - - - - - - - - - - - - - - - -
           IF OR-CPOTCHGOFF-CD NOT = CH-CPOTCHGOFF-CD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CON CHGOFF "       TO MEMO-FIELD
               MOVE "18#12"             TO MEMO-PG-FLD
               MOVE OR-CPOTCHGOFF-CD    TO MEMO-OR-CHAR
               MOVE CH-CPOTCHGOFF-CD    TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *RECENCY POTENTIAL - - - - - - - - - - - - - - - - - -
           IF OR-RDPOT30-CD NOT = CH-RDPOT30-CD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "REC POTENTL"       TO MEMO-FIELD
               MOVE "18#13"             TO MEMO-PG-FLD
               MOVE OR-RDPOT30-CD       TO MEMO-OR-CHAR
               MOVE CH-RDPOT30-CD       TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *RECENCY CHARGEOFF CODE- - - - - - - - - - - - - - - -
           IF OR-RDPOTCHGOFF-CD NOT = CH-RDPOTCHGOFF-CD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "REC CHGOFF "       TO MEMO-FIELD
               MOVE "18#14"             TO MEMO-PG-FLD
               MOVE OR-RDPOTCHGOFF-CD   TO MEMO-OR-CHAR
               MOVE CH-RDPOTCHGOFF-CD   TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      * SP-FEE TABLE       - - - - - - - - - - - - - - - - -
      * SP-FEETABLE           OCCURS 2.
      * FEE CODE FRMLA- - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF OR-FEEFRMLA(SUB) NOT = CH-FEEFRMLA(SUB)
                 COMPUTE FIELD-NO = 14 + SUB
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE SPACES              TO MEMO-FIELD
                 STRING "FEE FRML" "(" SUB ")"
                         DELIMITED BY "  " INTO MEMO-FIELD
                 MOVE 18                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO            TO MEMO-FLD-NUM
                 MOVE OR-FEEFRMLA(SUB)    TO MEMO-OR-CHAR
                 MOVE CH-FEEFRMLA(SUB)    TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      * FEE CODE - -  - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF OR-FEECD(SUB) NOT = CH-FEECD(SUB)
                 COMPUTE FIELD-NO = 14 + SUB
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE SPACES              TO MEMO-FIELD
                 STRING "CD" "(" SUB ")"
                         DELIMITED BY "  " INTO MEMO-FIELD
                 MOVE 18                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO            TO MEMO-FLD-NUM
                 MOVE OR-FEECD(SUB)       TO MEMO-OR-CHAR
                 MOVE CH-FEECD(SUB)       TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      * FEE ID   - -  - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF OR-FEEID(SUB) NOT = CH-FEEID(SUB)
                 COMPUTE FIELD-NO = 14 + SUB
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE SPACES              TO MEMO-FIELD
                 STRING "ID" "(" SUB ")"
                         DELIMITED BY "  " INTO MEMO-FIELD
                 MOVE 18                  TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO            TO MEMO-FLD-NUM
                 MOVE OR-FEEID(SUB)       TO MEMO-OR-CHAR
                 MOVE CH-FEEID(SUB)       TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
              END-IF
           END-PERFORM.

      *ALLOW BACKDATE OF LOANS - - - - - - - - - - - - - - - - - - - - -
           IF OR-ALLOW-LN-BACK-DATE NOT = CH-ALLOW-LN-BACK-DATE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "ALLOW BACKDT"      TO MEMO-FIELD
               MOVE "18#17"             TO MEMO-PG-FLD
               MOVE OR-ALLOW-LN-BACK-DATE TO MEMO-OR-CHAR
               MOVE CH-ALLOW-LN-BACK-DATE TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *MAX IB INTEREST ESTIMATE- - - - - - - - - - - - - - - - - - - - -
           IF OR-CAL-MAX-AT-ESTIMATE NOT = CH-CAL-MAX-AT-ESTIMATE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "MAX IB ESTIM"      TO MEMO-FIELD
               MOVE "18#18"             TO MEMO-PG-FLD
               MOVE OR-CAL-MAX-AT-ESTIMATE TO MEMO-OR-CHAR
               MOVE CH-CAL-MAX-AT-ESTIMATE TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-ALLOW-ZERO-PROC NOT = CH-ALLOW-ZERO-PROC
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "ALLOW 0 PROC"      TO MEMO-FIELD
               MOVE "18#19"             TO MEMO-PG-FLD
               MOVE OR-ALLOW-ZERO-PROC TO MEMO-OR-CHAR
               MOVE CH-ALLOW-ZERO-PROC TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-ADV-TO-1ST-NO-CORR NOT = CH-ADV-TO-1ST-NO-CORR
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "ADV 1ST NO C"      TO MEMO-FIELD
               MOVE "18#20"             TO MEMO-PG-FLD
               MOVE OR-ADV-TO-1ST-NO-CORR       TO MEMO-OR-CHAR
               MOVE CH-ADV-TO-1ST-NO-CORR       TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-LC-INT-ON-REPO-NON-PL NOT = CH-LC-INT-ON-REPO-NON-PL
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "LC/INT REPO "      TO MEMO-FIELD
               MOVE "18#21"             TO MEMO-PG-FLD
               MOVE OR-LC-INT-ON-REPO-NON-PL TO MEMO-OR-CHAR
               MOVE CH-LC-INT-ON-REPO-NON-PL TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-CONTRACTUAL-SET-NONACCRUAL NOT =
                                   CH-CONTRACTUAL-SET-NONACCRUAL
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CON NON ACC "      TO MEMO-FIELD
               MOVE "18#22"             TO MEMO-PG-FLD
               MOVE OR-CONTRACTUAL-SET-NONACCRUAL TO MEMO-OR-CHAR
               MOVE CH-CONTRACTUAL-SET-NONACCRUAL TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-CONTRACTUAL-RESET-ACCRUAL NOT =
                                   CH-CONTRACTUAL-RESET-ACCRUAL
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CON RESET AC"      TO MEMO-FIELD
               MOVE "18#23"             TO MEMO-PG-FLD
               MOVE OR-CONTRACTUAL-RESET-ACCRUAL TO MEMO-OR-CHAR
               MOVE CH-CONTRACTUAL-RESET-ACCRUAL TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-MONTHS-PAY-NAPP-COUNTER NOT =
                                    CH-MONTHS-PAY-NAPP-COUNTER
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "PY NAPP CNTR"      TO MEMO-FIELD
               MOVE "18#24"             TO MEMO-PG-FLD
               MOVE OR-MONTHS-PAY-NAPP-COUNTER TO MEMO-OR-CHAR
               MOVE CH-MONTHS-PAY-NAPP-COUNTER TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-SHORT-PAY-THRESHOLD NOT = CH-SHORT-PAY-THRESHOLD
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "SHORT PY THR"      TO MEMO-FIELD
               MOVE "18#25"             TO MEMO-PG-FLD
               MOVE OR-SHORT-PAY-THRESHOLD TO MEMO-OR-CHAR
               MOVE CH-SHORT-PAY-THRESHOLD TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-CASH-AVAIL-PERCENT NOT = CH-CASH-AVAIL-PERCENT
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CASH AVAIL"        TO MEMO-FIELD
               MOVE "18#26"             TO MEMO-PG-FLD
               MOVE OR-CASH-AVAIL-PERCENT TO MEMO-OR-CHAR
               MOVE CH-CASH-AVAIL-PERCENT TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-NET-BAL-REQ NOT = CH-NET-BAL-REQ
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CASH AVAIL"        TO MEMO-FIELD
               MOVE "18#27"             TO MEMO-PG-FLD
               MOVE OR-NET-BAL-REQ TO MEMO-OR-CHAR
               MOVE CH-NET-BAL-REQ TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-AMT-PROCEEDS-REQ NOT = CH-AMT-PROCEEDS-REQ
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "%NETBAL REQ"        TO MEMO-FIELD
               MOVE "18#28"             TO MEMO-PG-FLD
               MOVE OR-AMT-PROCEEDS-REQ TO MEMO-OR-CHAR
               MOVE CH-AMT-PROCEEDS-REQ TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

           IF OR-ALT-PREPAY-MONTHS NOT = CH-ALT-PREPAY-MONTHS
              MOVE SPACES                TO MEMO-OR-CHAR
              MOVE SPACES                TO MEMO-CH-CHAR
              MOVE "PREPAY MO"           TO MEMO-FIELD    
              MOVE "18#29"               TO MEMO-PG-FLD   
              MOVE  OR-ALT-PREPAY-MONTHS TO MEMO-OR-CHAR
              MOVE  CH-ALT-PREPAY-MONTHS TO MEMO-CH-CHAR    
              PERFORM CREATE-SM-MEMO.    



      * PAGE 19 INSURACE TABLE INCLUDED WITH OTHER INSURANCES
      * PAGE 20

      *MINIMUM CREDIT SCORE- - - - - - - - - - - - - - - - -
             IF OR-MIN-CREDIT-SCORE NOT = CH-MIN-CREDIT-SCORE
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "MIN CRED SC"       TO MEMO-FIELD
               MOVE "20#01"             TO MEMO-PG-FLD
               MOVE OR-MIN-CREDIT-SCORE TO MEMO-OR-DEC
               MOVE CH-MIN-CREDIT-SCORE TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *PACESETTER FLAG (SINGLE PAYS) - - - - - - - - - - - -
             IF OR-PACESETTER-FG NOT = CH-PACESETTER-FG
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "SINGLE PAY "       TO MEMO-FIELD
               MOVE "20#02"             TO MEMO-PG-FLD
               MOVE OR-PACESETTER-FG    TO MEMO-OR-CHAR
               MOVE CH-PACESETTER-FG    TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *WAGE ASSIGNMENT DAYS- - - - - - - - - - - - - - - - -
             IF OR-WAGE-ASSIGN-DAYS NOT = CH-WAGE-ASSIGN-DAYS
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "WAGE ASSIGN"       TO MEMO-FIELD
               MOVE "20#03"             TO MEMO-PG-FLD
               MOVE OR-WAGE-ASSIGN-DAYS TO MEMO-OR-NUM
               MOVE CH-WAGE-ASSIGN-DAYS TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *ROUND PAYMENT (CALCULATORS) - - - - - - - - - - - - -
             IF OR-CAL-PAYMENT-ROUND-FG NOT = CH-CAL-PAYMENT-ROUND-FG
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "CALC PMT FG"       TO MEMO-FIELD
               MOVE "20#04"             TO MEMO-PG-FLD
               MOVE OR-CAL-PAYMENT-ROUND-FG TO MEMO-OR-CHAR
               MOVE CH-CAL-PAYMENT-ROUND-FG TO MEMO-CH-CHAR
               PERFORM CREATE-SM-MEMO.

      *NUMBER OF PAYS TILL RENEWABLE - - - - - - - - - - - - - - - - - -
           IF OR-RENEWABLE-PAYS NOT = CH-RENEWABLE-PAYS
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "NO PAYS RENW"      TO MEMO-FIELD
               MOVE "20#05"             TO MEMO-PG-FLD
               MOVE OR-RENEWABLE-PAYS   TO MEMO-OR-NUM
               MOVE CH-RENEWABLE-PAYS   TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

      *NUMBER OF DAYS TILL RENEWABLE - - - - - - - - - - - - - - - - - -
           IF OR-RENEWABLE-DAYS NOT = CH-RENEWABLE-DAYS
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "NO DAYS RENW"      TO MEMO-FIELD
               MOVE "20#06"             TO MEMO-PG-FLD
               MOVE OR-RENEWABLE-DAYS TO MEMO-OR-NUM
               MOVE CH-RENEWABLE-DAYS TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

           IF OR-MAX-CASH-PROCEEDS NOT = CH-MAX-CASH-PROCEEDS
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "MAX CASH PROC"      TO MEMO-FIELD
               MOVE "20#07"             TO MEMO-PG-FLD
               MOVE OR-MAX-CASH-PROCEEDS TO MEMO-OR-DEC
               MOVE CH-MAX-CASH-PROCEEDS TO MEMO-CH-DEC
               PERFORM CREATE-SM-MEMO.

      *EXPIRATION DATE FORMULA - - - - - - - - - - - - - - - - - - -
           IF  OR-EXPD-FRMLA NOT = CH-EXPD-FRMLA
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "EXP DT FRM "       TO MEMO-FIELD
              MOVE "20#17"             TO MEMO-PG-FLD
              MOVE OR-EXPD-FRMLA       TO MEMO-OR-CHAR
              MOVE CH-EXPD-FRMLA       TO MEMO-CH-CHAR
              PERFORM CREATE-SM-MEMO.

      *EXPIRATION MONTHS - - - - - - - - - - - - - - - - - - - - - -
           IF  OR-EXPD-MONTHS NOT = CH-EXPD-MONTHS
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "EXP MONTHS "       TO MEMO-FIELD
              MOVE "20#18"             TO MEMO-PG-FLD
              MOVE OR-EXPD-MONTHS      TO MEMO-OR-NUM
              MOVE CH-EXPD-MONTHS      TO MEMO-CH-NUM
              PERFORM CREATE-SM-MEMO.

      *EXPIRATION DAYS - - - - - - - - - - - - - - - - - - - - - - -
           IF  OR-EXPD-DAYS NOT = CH-EXPD-DAYS
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "EXP DAYS   "       TO MEMO-FIELD
              MOVE "20#19"             TO MEMO-PG-FLD
              MOVE OR-EXPD-DAYS        TO MEMO-OR-NUM
              MOVE CH-EXPD-DAYS        TO MEMO-CH-NUM
              PERFORM CREATE-SM-MEMO.

      *EXPIRATION EXCEPTION- - - - - - - - - - - - - - - - - - - - -
           IF  OR-EXPD-EXCEPT NOT = CH-EXPD-EXCEPT
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "EXP DT EXCP"       TO MEMO-FIELD
              MOVE "20#20"             TO MEMO-PG-FLD
              MOVE OR-EXPD-EXCEPT      TO MEMO-OR-CHAR
              MOVE CH-EXPD-EXCEPT      TO MEMO-CH-CHAR
              PERFORM CREATE-SM-MEMO.

      *EXPIRATION RATE FORMULA - - - - - - - - - - - - - - - - - - -
           IF OR-EXPR-FRMLA NOT = CH-EXPR-FRMLA
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "EXP RATE FM"       TO MEMO-FIELD
              MOVE "20#21"             TO MEMO-PG-FLD
              MOVE OR-EXPR-FRMLA       TO MEMO-OR-CHAR
              MOVE CH-EXPR-FRMLA       TO MEMO-CH-CHAR
              PERFORM CREATE-SM-MEMO.

      *EXPIRATION LATE CHARGE  - - - - - - - - - - - - - - - - - - -
           IF OR-EXPR-LCHG NOT = CH-EXPR-LCHG
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "EXP LC CD  "       TO MEMO-FIELD
              MOVE "20#22"             TO MEMO-PG-FLD
              MOVE OR-EXPR-LCHG TO MEMO-OR-CHAR
              MOVE CH-EXPR-LCHG TO MEMO-CH-CHAR
              PERFORM CREATE-SM-MEMO.

      *EXPIRATION REDUCTION FORMULA- - - - - - - - - - - - - - - - -
           IF OR-EXPR-REDU-FRMLA NOT = CH-EXPR-REDU-FRMLA
              MOVE SPACES              TO MEMO-OR-CHAR
              MOVE SPACES              TO MEMO-CH-CHAR
              MOVE "EXP REDU FM"       TO MEMO-FIELD
              MOVE "20#23"             TO MEMO-PG-FLD
              MOVE OR-EXPR-REDU-FRMLA  TO MEMO-OR-CHAR
              MOVE CH-EXPR-REDU-FRMLA  TO MEMO-CH-CHAR
              PERFORM CREATE-SM-MEMO.

      *EXPIRATION RATE TABLE   (5 OCCURENCES OF EACH FIELD)- - - - -
      *    SP-EXPR-RATETBL                OCCURS 5
      *    SP-EXPR-RATE

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               COMPUTE FIELD-NO = 23 + SUB
               IF OR-EXPR-RATE(SUB) NOT =  CH-EXPR-RATE(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE   "EXPR RATE  "     TO MEMO-FIELD
                 MOVE  20                 TO MEMO-PG
                 MOVE "#"                TO MEMO-SEP
                 MOVE FIELD-NO               TO MEMO-FLD-NUM
                 MOVE OR-EXPR-RATE(SUB)  TO MEMO-OR-DEC3
                 MOVE CH-EXPR-RATE(SUB)  TO MEMO-CH-DEC3
                 PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.

      *    SP-EXPR-BREAK
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               COMPUTE FIELD-NO = 23 + SUB
               IF OR-EXPR-BREAK(SUB) NOT =  CH-EXPR-BREAK(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE   "EXPR BREAK "     TO MEMO-FIELD
                 MOVE  20                 TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO            TO MEMO-FLD-NUM
                 MOVE OR-EXPR-BREAK(SUB)  TO MEMO-OR-DEC
                 MOVE CH-EXPR-BREAK(SUB)  TO MEMO-CH-DEC
                 PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.


      *    SP-EXPR-YRTYPE
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               COMPUTE FIELD-NO = 23 + SUB
               IF OR-EXPR-YRTYPE(SUB) NOT =  CH-EXPR-YRTYPE(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE   "EXP YR TYP "     TO MEMO-FIELD
                 MOVE  20                 TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO                TO  MEMO-FLD-NUM
                 MOVE OR-EXPR-YRTYPE(SUB)  TO MEMO-OR-CHAR
                 MOVE CH-EXPR-YRTYPE(SUB)  TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.

      *    SP-EXPR-INTMETH
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               COMPUTE FIELD-NO = 23 + SUB
             IF OR-EXPR-INTMETH(SUB) NOT =  CH-EXPR-INTMETH(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE   "EXP INTEREST"    TO MEMO-FIELD
                 MOVE  20                 TO MEMO-PG
                 MOVE "#"                 TO MEMO-SEP
                 MOVE FIELD-NO                TO MEMO-FLD-NUM
                 MOVE OR-EXPR-INTMETH(SUB)  TO MEMO-OR-CHAR
                 MOVE CH-EXPR-INTMETH(SUB)  TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
               END-IF
           END-PERFORM.


      * PAGE 23

      *REPO FORM TABLE - - - - - - - - - - - - - - - - - - - - - - - - -
      *SP-REPO-TRCD-FMNO-TABLE              OCCURS 3 TIMES.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
             IF OR-REPO-TRCD(SUB) NOT = CH-REPO-TRCD(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE SPACES              TO MEMO-FIELD
                 STRING "TRCD", " ", SUB DELIMITED BY "  "
                                         INTO MEMO-FIELD
                 MOVE "23#01"             TO MEMO-PG-FLD
                 MOVE  OR-REPO-TRCD(SUB)  TO MEMO-OR-CHAR
                 MOVE  CH-REPO-TRCD(SUB)  TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
             IF OR-AUTO-SEND-TRCD(SUB) NOT = CH-AUTO-SEND-TRCD(SUB)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE SPACES              TO MEMO-FIELD
                 STRING "TRCD", " ", SUB DELIMITED BY "  "
                                         INTO MEMO-FIELD
                 MOVE "23#01"             TO MEMO-PG-FLD
                 MOVE  OR-AUTO-SEND-TRCD(SUB)  TO MEMO-OR-CHAR
                 MOVE  CH-AUTO-SEND-TRCD(SUB)  TO MEMO-CH-CHAR
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

      *4 FORM NUMBERS ASSOCIATED WITH EACH TRAN CODE - - - - - - - - -
      *SP-REPO-FMNO     OCCURS 4.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 4
             IF OR-REPO-FMNO(SUB,S1) NOT = CH-REPO-FMNO(SUB,S1)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE SPACES              TO MEMO-FIELD
                 STRING "FMNO", "(",  SUB "," S1, ")"
                     DELIMITED BY "  " INTO MEMO-FIELD
                 MOVE "23#01"             TO MEMO-PG-FLD
                 MOVE  OR-REPO-FMNO(SUB,S1)  TO MEMO-OR-NUM
                 MOVE  CH-REPO-FMNO(SUB,S1)  TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
                    AFTER   S1 FROM 1 BY 1 UNTIL S1 > 4
             IF OR-AUTO-SEND-FMNO(SUB,S1) NOT =
                                   CH-AUTO-SEND-FMNO(SUB,S1)
                 MOVE SPACES              TO MEMO-OR-CHAR
                 MOVE SPACES              TO MEMO-CH-CHAR
                 MOVE SPACES              TO MEMO-FIELD
                 STRING "FMNO", "(",  SUB "," S1, ")"
                     DELIMITED BY "  " INTO MEMO-FIELD
                 MOVE "23#01"             TO MEMO-PG-FLD
                 MOVE  OR-AUTO-SEND-FMNO(SUB,S1)  TO MEMO-OR-NUM
                 MOVE  CH-AUTO-SEND-FMNO(SUB,S1)  TO MEMO-CH-NUM
                 PERFORM CREATE-SM-MEMO
             END-IF
           END-PERFORM.

           IF OR-PERCENT-REGPAY-AVAIL NOT = CH-PERCENT-REGPAY-AVAIL
               MOVE SPACES              TO MEMO-OR-CHAR
               MOVE SPACES              TO MEMO-CH-CHAR
               MOVE "% REGPAY AV"       TO MEMO-FIELD
               MOVE "23#04"             TO MEMO-PG-FLD
               MOVE OR-PERCENT-REGPAY-AVAIL TO MEMO-OR-NUM
               MOVE CH-PERCENT-REGPAY-AVAIL TO MEMO-CH-NUM
               PERFORM CREATE-SM-MEMO.

            IF OR-NON-LOAN-FLAG NOT = CH-NON-LOAN-FLAG
              MOVE SPACES                TO MEMO-OR-CHAR
              MOVE SPACES                TO MEMO-CH-CHAR 
              MOVE "NON LOAN FG"         TO MEMO-FIELD
              MOVE "23#05"               TO MEMO-PG-FLD
              MOVE  OR-NON-LOAN-FLAG     TO MEMO-OR-CHAR
              MOVE  CH-NON-LOAN-FLAG     TO MEMO-CH-CHAR
              PERFORM CREATE-SM-MEMO.    

            IF OR-SKIP-TRW NOT = CH-SKIP-TRW
              MOVE SPACES                TO MEMO-OR-CHAR
              MOVE SPACES                TO MEMO-CH-CHAR 
              MOVE "SKIP TRW "           TO MEMO-FIELD
              MOVE "23#06"               TO MEMO-PG-FLD
              MOVE  OR-SKIP-TRW          TO MEMO-OR-CHAR
              MOVE  CH-SKIP-TRW          TO MEMO-CH-CHAR
              PERFORM CREATE-SM-MEMO.    


       UPDATE-CHANGES-EXIT.
           EXIT.

      ********************************************
       CREATE-SM-MEMO SECTION.
      ********************************************
           MOVE CH-ORGST            TO HOLD-SM-SP-ORGST.
           MOVE CH-SPRCLASS         TO HOLD-SM-SP-SPRCLASS.
           MOVE CH-SUBCLASS         TO HOLD-SM-SP-SUBCLASS.
           MOVE CH-LAWCODE          TO HOLD-SM-SP-LAWCODE.
           MOVE ZEROS               TO HOLD-SM-DATE.
           MOVE ZEROS               TO HOLD-SM-SEQ.

           PERFORM GETDAT.
           COMPUTE HOLD-SM-DATE = 99999999 - DATE-YYYYMMDD.

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO MEMO-TIME.

           MOVE EXT-CONAME-USER-ID TO MEMO-USERID.

           MOVE 1  TO HOLD-SM-SEQ.
           PERFORM OPEN-SM1-FILE.

       UPDATE-SM-FILE-AGAIN.

           MOVE SPACES TO SM-REC.

           MOVE HOLD-SM1-KEY TO SM1-KEY.
           MOVE "A " TO SM-MEMO-TYPE.

           MOVE MEMO-AREA TO SM-MEMO.

           PERFORM WRITE-SM1-FILE.
           IF IO-BAD
              IF E-CODE NOT = "22"
                 GO TO IO-ERROR
           ELSE
              IF HOLD-SM-SEQ < 999
                 ADD 1 TO HOLD-SM-SEQ
                 GO TO UPDATE-SM-FILE-AGAIN.

       CREATE-SM-MEMO-EXIT.
           EXIT.


      ********************************************
       GET-INS-TYP-DESC SECTION.
      ********************************************
           IF SUB = 1
              MOVE 05    TO MEMO-PG
              MOVE "CL  " TO MEMO-INS-TY
           ELSE
           IF SUB = 2
              MOVE 06    TO MEMO-PG
              MOVE "CLJ" TO MEMO-INS-TY
           ELSE
           IF SUB = 3
              MOVE 07    TO MEMO-PG
              MOVE "LL  " TO MEMO-INS-TY
           ELSE
           IF SUB = 4
              MOVE 08    TO MEMO-PG
              MOVE "LLJ " TO MEMO-INS-TY
           ELSE
           IF SUB = 5
              MOVE 09    TO MEMO-PG
              MOVE "AH  " TO MEMO-INS-TY
           ELSE
           IF SUB = 6
              MOVE 10    TO MEMO-PG
              MOVE "PP  " TO MEMO-INS-TY
           ELSE
           IF SUB = 7
              MOVE 11    TO MEMO-PG
              MOVE "PP2 " TO MEMO-INS-TY
           ELSE
           IF SUB = 8
              MOVE 19    TO MEMO-PG
              MOVE "OT1 " TO MEMO-INS-TY.
       GET-INS-TYP-DESC-EXIT.
           EXIT.


      ********************************************
       GET-REBATE-TYP-DESC SECTION.
      ********************************************
           IF SUB = 1
              MOVE "CLSD "      TO MEMO-REB-TY
           ELSE
           IF SUB = 2
              MOVE "CLJD "      TO MEMO-REB-TY
           ELSE
           IF SUB = 3
              MOVE "CLSL "      TO MEMO-REB-TY
           ELSE
           IF SUB = 4
              MOVE "CLJL "      TO MEMO-REB-TY
           ELSE
           IF SUB = 5
              MOVE "A&H  "      TO MEMO-REB-TY
           ELSE
           IF SUB = 6
              MOVE "PP   "      TO MEMO-REB-TY
           ELSE
           IF SUB = 7
              MOVE "INT  "      TO MEMO-REB-TY
           ELSE
           IF SUB = 8
              MOVE "S/C  "      TO MEMO-REB-TY
           ELSE
           IF SUB = 9
              MOVE "M/ F "      TO MEMO-REB-TY
           ELSE
           IF SUB = 10
              MOVE "DEFER"      TO MEMO-REB-TY
           ELSE
           IF SUB = 11
              MOVE "NOT  "      TO MEMO-REB-TY
           ELSE
           IF SUB = 12
              MOVE "OTH1 "      TO MEMO-REB-TY
           ELSE
           IF SUB = 13
              MOVE "OTH2 "      TO MEMO-REB-TY
           ELSE
           IF SUB = 14
              MOVE "OTH3 "      TO MEMO-REB-TY.

       GET-REBATE-TYP-DESC-EXIT.
           EXIT.


      ********************************************
       GET-EARN-TYP-DESC SECTION.
      ********************************************
           IF SUB = 1
              MOVE 14          TO MEMO-PG
              MOVE "INT "      TO MEMO-EARN-TY
           ELSE
           IF SUB = 2
              MOVE 15          TO MEMO-PG
              MOVE "ADD "      TO MEMO-EARN-TY
           ELSE
           IF SUB = 3
              MOVE 15          TO MEMO-PG
              MOVE "SC  "      TO MEMO-EARN-TY
           ELSE
           IF SUB = 4
              MOVE 16          TO MEMO-PG
              MOVE "MF  "      TO MEMO-EARN-TY
           ELSE
           IF SUB = 5
              MOVE 16          TO MEMO-PG
              MOVE "DSC "      TO MEMO-EARN-TY
           ELSE
           IF SUB = 6
              MOVE 17          TO MEMO-PG
              MOVE "OTH "      TO MEMO-EARN-TY.


       COPY "LIBGB/DATER.CPY".

      **********************
       MISC-ROUTINES SECTION.
      **********************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETDAT.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SM1-FILE.
       COPY "LIBLP/LPSMGS_SQL.CPY".
       COPY "LIBLP/LPSM1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
