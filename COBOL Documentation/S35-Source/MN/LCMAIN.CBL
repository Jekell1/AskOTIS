       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LCMAIN.
      ******************************************************************
      *
      *  DESC:   LOAN CLASS FILE MAINTENANCE
      *
      * REV:
      *  MJD 040896 ADDED LC-AFP-CLASS.
      *  JTG 071896 ADDED LC-CON-PDUE-DAYS & LC-REC-PDUE-DAYS
      *  JTG 090597 ADDED LC-GIFTNET LOGIC RE: GP-I-AM-WORLD
      *  GLF 091897 REPLACED ANY PROMPT THAT DISPLAYS WORD 'GIFTNET' W/'WCBC'
      *   CS 031098 ADDED LC-APP-REQ-FOR-LOAN
      *  KEC 090298 ADDED LC-REVOLVING; HAD TO ADD CHECKS IN
      *              'VALID-COMBINATION' SECTION.
      * JTG 101298 ADDED TEST ON GP-ALLOW-REVOLVING-FG
      *  MJD 990722 FIXED AFP-DEALER/CLASS TEST TO COUNT " " AS "N".
      *  MG  990909 DELETED AFP-DEALER
      *  BLV 000217 ALLOW "W" FOR LC-CALC-HI-CREDIT, WORLD PR# 225
      *  CS  010314 ADDED LC-POOLID (DEFAULT POOL ID) MERC
      *  CS  010320 ADDED SCAN ON LC-POOLID.
      *  BAH 030423 ADDED LC-USE-ALT-GL-FEE (18) FOR LENDMARK PR# 2048
      *  BLV 030730 ADD FIELD 19, PROGRESS REPORT CLASS, EXCEL PR# 2098
      *  BLV 041112 IGNORE VOIDED LOANS WHEN CHECKING IF LOANS EXIST
      *             FOR DELETE OR CHANGING FIELDS 2 - 6
      *  AMM 070301 CREATED A LOG FIELD CHANGE ROUTINE
      *  BAH 071009 ADDED FIELD 20, ALLOW CASH PROCEEDS, LC-ALLOW-CASH-PROCEEDS
      *             REGMGM PR# 3074
      *  BAH 081003 ALLOW LC-STOP-ON-DLNO TO BE "R" FOR REGENCY TO STOP
      *             IN DIRECT LOAN INPUT AND FORCE ANYTHING BUT 9999.99
      *                REGENCY REGWAN PR# 3259
      *  BAH 121001 ADDED LC-SALES-FORCED-ENTRY REDEFINES LC-STOP-ON-DLNO TO
      *             FORCE RESERVE/DISCOUNT FIELD ENTRIES IN SALES INPUT,
      *                                          SECURITY AUTO PR# 4116
      *  MJD 121217 ADDED WINDOW LOGIC AND LCMAIN-WINDOW-HANDLE.
      *  MJD 130115 CLEANED UP PROGRAM, REMOVED       LINES THAT WERE COMMENTED
      *             OUT DUE TO CHANGES MADE AFTER THE INITIAL CONVERSION.
      *  BAH 130111 ADDED LC-DISPLAY-ERN OPTION OF "I" FOR TODAY DUE
      *             INTEREST (POFF-INTDUE), AMERICAN ACC PR# 4190
      *  BAH 151015 EXPANDED LC-DESC, EXPANDED RECORD, REMOVE REDEFINE
      *              A30 CONVERSION, PD0003
      *
      * KEC 2016.0311 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-POOL???? TO CD-PL-????
      *
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      *  MJD 160823 ADDED "L" AS A VALID HIGH CREDIT LIMIT OPTION.
      *             THIS WILL WORK JUST AS "C" EXCEPT ONLY NON-MEMO FEES
      *             STARTING WITH A NUMERIC WILL BE SUBTRACTED FROM 
      *             CASH ADVANCE WHEN CALCULATING HIGH CREDIT. THIS IS
      *             SPECIFIC TO LIVE CHECKS WHERE THE PROCEEDS ARE ZERO
      *             AND THE LOAN AMOUNT IS PUT ON AS A FEE CODE. 61000/1180
      * BAH 170228 COULD NOT GET TO FIELD 21, NEEDED TO CHANGE SKIP FUTURE
      *            FIELD 'IF' TO BE > 21
      * BAH 181211 GLOBAL LNFILE
      * BAH 20190625 REMOVED LC-REVOLVING, 4. REVOLVING, PT#0153
      * BAH 20190718 ADDED CLEAR-LC-FILE (LPLCCL) FUTURE FIELDS NOT SET
      * BAH 20220808 HARD CODED START
      * BAH 20230622 STOP "W" FOR LC-CALC-HI-CREDIT
      * BAH 2024.0610 NO CHANGE BUT NEEDED TO SEND FOR THE SCAN TO WORK?
      *               NO IDEA WHY DIFFERENT S35Q-62
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      /*****************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/LCMAIN.CBL S35 02/21/24-12:30:53 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

      *-----------------------------------------------------------------
       01  HOLD-LC1-KEY                PIC 9(3).

      *=================================================================
      *   LCCH-REC FROM 'REPLACING LEADING' ON LC-REC. (CHANGED-REC)
      *   LCOR-REC FROM 'REPLACING LEADING' ON LC-REC. (ORIGINAL-REC)
      *=================================================================
       COPY "LIBLP/LP01LC.CPY" REPLACING LEADING "LC" BY "LCOR".
       COPY "LIBLP/LP01LC.CPY" REPLACING LEADING "LC" BY "LCCH".

      *=================================================================
       01  ERRCD           PIC X VALUE " ".
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).

       01  CHAN-DATA       VALUE SPACES.
           03  CHAN        PIC X(4).

       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-LC-CODE      PIC 9(3).
           03  FILLER                   PIC X(11).
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(8).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(16).
               05  WORK-ERRLOG-ORG-ALP2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC X.
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC ZZZZ9-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z9-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(16).
               05  WORK-ERRLOG-CHG-ALP2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC X.
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC ZZZZ9-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z9-.

       01  ELE             PIC 9(3).

       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  ONE             PIC S9.
       01  VALID-FG        PIC X       VALUE SPACES.

      ******************************************************************
      *     SCREEN DISPLAY BUFFER
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(30).
           03  F02         PIC X.
           03  F03         PIC X.
           03  F04         PIC X.
           03  F05         PIC X.
           03  F06         PIC X.
           03  F07         PIC X.
           03  F08         PIC X.
           03  F09         PIC X.
           03  F10         PIC X.
           03  F11         PIC X.
           03  F12         PIC X.
           03  F13         PIC X.
           03  F14         PIC X.
           03  F15         PIC X.
           03  F16         PIC X.
           03  F17         PIC ZZZZ9.
           03  F18         PIC X.
           03  F19         PIC Z9.
           03  F20         PIC X.
           03  F21         PIC X.
           03  F99         PIC X.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(44) VALUE
           "010 020 030 040 050 060 070 080 090 100 110 ".
           03  FILLER      PIC X(44) VALUE
           "120 130 140 150 160 170 180 190 200 210 990.".

      ******************************************************************
      *     ELEMENT SPECIFICATIONS
      ******************************************************************
       01  MAX             PIC 99   VALUE 99.
       01  SPEC-TABLE.
      * 1. LOAN CLASS DESCRIPTION
           03  FILLER      PIC X(8) VALUE "E  A300 ".
      * 2. SMALL LOAN FLAG (Y/N)
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 3. SALES FINANCE FLAG (Y/N)
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 4. N/A
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 5. SERVICE ACCOUNT FLAG (Y/N)
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 6. LOAN TYPE: I=INTEREST BEARING   P=PRECOMPUTED
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 7. LEDGER CARD SWITCH
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 8. DELINQ PROFILE1
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 9. DELINQ PROFILE2
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *10. DISPLAY EARNINGS
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *11. CALCULATE HIGH CREDIT
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *12. SKIP CREDIT BUREAU
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *13. STOP ON DEALER NUMBER 
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *14. CON OR DAYS PAST DUE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *15. REC OR DAYS PAST DUE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *16  APP REQUIRED FOR LOAN IN THIS CLASS
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *17  DEFAULT POOL ID
           03  FILLER      PIC X(8) VALUE "EN N050 ".
      *18  USE ALT FEE CODE GL
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *19  PROGRESS REPORT LINE
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *20  ALLOW CASH PROCEED
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *21. SALES FORCED ENTRY FOR GP-I-AM SECURITY
           03  FILLER      PIC X(8) VALUE "E  A010 ".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 21 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

      *99. WCBC FLAG:
       01  GIFTNET-SPEC    PIC X(8) VALUE "E  A010 ".

      /*****************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       01  LCMAIN-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LCMAIN_DEF.CPY".
       COPY "LIBMN/LCMAIN_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBMN/LCMAIN_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N   P R O G R A M    M O D U L E
      *
      ******************************************************************
       A000-MAIN-PROGRAM SECTION.
      D    STOP MESS.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "LCMAIN" TO VDU-FORM-NAME.
           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LCMAIN-WINDOW-HANDLE.

           PERFORM GET-GPENV.
           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

       A010-MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE "S  A180P99A:S." TO VDU.
           MOVE "99. WCBC CLASS   :" TO VDUBUF..
           PERFORM SCREENIO.
           MOVE "S  A010990." TO VDU.
           MOVE "N" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A180P99B:S." TO VDU.
           MOVE "(Y/N)" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM B010-REC-LC-MODULE.

           IF ( IO-TYPE = "END" )
              GO TO END-ROUTINE.

           PERFORM B020-ENTRY-MODULE.

           IF ( IO-TYPE = "RWR" )
              PERFORM B030-WRITE-MODULE.

           IF ( IO-TYPE = "DEL" )
             PERFORM B040-DELETE-MODULE.

           GO TO A010-MAIN-MODULE.

      ******************************************************************
       B010-REC-LC-MODULE SECTION.
      ******************************************************************
       B010-REC-LC-MODULE-NO.
           MOVE 0      TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE "F7-EXIT, F6-SCAN, ENTER LOAN CLASS #:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "EN N020CLASS." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1<" )
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO B010-REC-LC-MODULE-NO
              ELSE
                 MOVE "SN N020CLASS." TO VDU
                 MOVE LCSCAN-KEY TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE LCSCAN-KEY TO VDUNUM0.

           IF ( VDUSTATUS = "1>" )
             MOVE "END" TO IO-TYPE
             GO TO END-B010-REC-LC-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO B010-REC-LC-MODULE-NO.

           IF ( VDUNUM0 = 0 )
              GO TO B010-REC-LC-MODULE-NO.

           MOVE VDUNUM0 TO LC-CODE HOLD-LC1-KEY.

      *-----------------------------------------------------------------
       READ-LCFILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF ( IO-FG = 0 )
              MOVE LC-REC TO LCOR-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-B010-REC-LC-MODULE.
           EXIT.

      ******************************************************************
       CLASS-SCAN SECTION.
      ******************************************************************

           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-CLASS-SCAN.

           MOVE 0               TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ********************************************
       CODE-SCAN SECTION.
      ********************************************

           MOVE "PL"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-PL-POOLID.
           MOVE SPACES TO CDSCAN-PL-FILLER.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE 0 TO IO-FG.

           MOVE "SNNN050170." TO VDU.
           MOVE CDSCAN-PL-POOLID TO VDUNUM0.

           PERFORM SCREENIO.

           MOVE CDSCAN-PL-POOLID TO VDUNUM0.

       EXIT-CODE-SCAN.
           EXIT.

      ******************************************************************
      *      NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                 1 - INITIAL NEW RECORD ENTRY
      *                 2 - MODIFY NEW RECORD
      ******************************************************************
       B020-ENTRY-MODULE SECTION.
           MOVE SPACES  TO IO-TYPE.
           MOVE 1       TO ONE.

       B020-CREATE-ROUTINE.
           IF ( NEW-FLAG NOT = 1 )
              GO TO B020-MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE < 1 )
              MOVE 1 TO ELE ONE.

           IF ( ELE NOT > MAX )
              GO TO B020-ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       B020-MODIFY-ROUTINE.
           MOVE "F1, 'DELETE', ENTER FIELD # :" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-B020-ENTRY-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO B020-MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ( ACTION = SPACES )
              MOVE "RWR" TO IO-TYPE
              GO TO END-B020-ENTRY-MODULE.

           IF ( ACTION = "DELETE" )
              MOVE "DEL" TO IO-TYPE
              GO TO END-B020-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF ( CHNO NOT NUMERIC )
              GO TO B020-MODIFY-ROUTINE.

           IF ( CHNO = 0 )
              GO TO B020-MODIFY-ROUTINE.

           IF ( CHNO > MAX )
              GO TO B020-MODIFY-ROUTINE.

           IF ( NEW-FLAG = 0 )
               IF ( CHNO > 1 AND < 7 )
                  PERFORM CHECK-LOANS
                  IF ( IO-TYPE = "CLR" )
                     GO TO END-B020-ENTRY-MODULE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       B020-ENTER-ELE.

      * SKIP FUTURE FIELDS:
           IF ELE > 21 AND < 99
              GO TO B020-CREATE-ROUTINE.

           IF ( CHKEY NOT = ZEROS )
              IF ( CHFG (ELE) = " " )
                 IF ( NEW-FLAG NOT = 1 )
                    GO TO B020-MODIFY-ROUTINE.

           IF ( CHFG(ELE) = "N" )
              IF ( NEW-FLAG = 1 )
                 GO TO B020-CREATE-ROUTINE
              ELSE
                 IF ( CHKEY NOT = CHAN )
                    GO TO B020-MODIFY-ROUTINE.

           IF ELE = 1
              MOVE "F1-CLR, ENTER CLASS DESCRIPTION:" TO MESS
              PERFORM SEND-LEGEND.

           IF ELE = 2
              MOVE "F1-CLR, IS THIS A SMALL LOAN CLASS Y/N:" TO MESS
              PERFORM SEND-LEGEND.

           IF ELE = 3
              MOVE "F1-CLR, IS THIS A SALES FINANCE CLASS Y/N:" TO MESS
              PERFORM SEND-LEGEND.

           IF ELE = 5
              MOVE "F1-CLR, IS THIS A SERVICE CLASS Y/N:" TO MESS
              PERFORM SEND-LEGEND.

           IF ELE = 6
              MOVE "F1-CLR, INTEREST BEARING OR PRECOMPUTED I/P:"
                                                           TO MESS
              PERFORM SEND-LEGEND.

           IF ELE = 7
              MOVE "F1-CLR, POST TO LEDGER CARDS Y/N:" TO MESS
              PERFORM SEND-LEGEND.

           IF ELE = 16
              MOVE "IS APPLICATION REQUIRED FOR THIS CLASS" TO MESS
              PERFORM SEND-LEGEND.

           IF ELE = 17
              MOVE "ENTER DEFAULT POOL FOR THIS CLASS " TO MESS
              PERFORM SEND-LEGEND.

           IF ELE = 99
              MOVE "F1-CLR, IS THIS A WCBC CLASS Y/N:" TO MESS
              PERFORM SEND-LEGEND.

           IF ELE = 99
              MOVE GIFTNET-SPEC TO VDU
           ELSE
              MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE 0         TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       B020-TEST-F4.
           IF ( VDUSTATUS = "1U" )
             IF ( NEW-FLAG = 1 )
              MOVE -1 TO ONE
              GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       B020-TEST-F5.
           IF ( VDUSTATUS = "1D" )
              MOVE 1 TO ONE
              GO TO B020-GOTO-FIELD.

      *-----------------------------------------------------------------
       B020-TEST-F6.
           IF ( VDUSTATUS NOT = "1<" )
              GO TO B020-TEST-F1.

           IF ELE = 17
              PERFORM CODE-SCAN.


      *-----------------------------------------------------------------
       B020-TEST-F1.
           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-B020-ENTRY-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO B020-ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       B020-GOTO-FIELD.

           IF ELE = 99
              GO TO F-99.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07
                      F-08 F-09 F-10 F-11 F-12 F-13
                      F-14 F-15 F-16 F-17 F-18 F-19
                      F-20 F-21
                 DEPENDING ON ELE.

           GO TO END-B020-ENTRY-MODULE.
      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS
      *
      *-----------------------------------------------------------------
       F-01.
           MOVE VDUBUF TO LC-DESC.
           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO LC-SMALLOAN.

           IF NOT ( LC-SMALLOAN = "Y" OR "N" )
              PERFORM INVALID-CODE
              GO TO B020-ENTER-ELE.

           PERFORM VALID-COMBINATION.

           IF ( VALID-FG = "N" )
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO LC-SALEFIN.

           IF NOT ( LC-SALEFIN = "Y" OR "N" )
              PERFORM INVALID-CODE
              GO TO B020-ENTER-ELE.

           PERFORM VALID-COMBINATION.

           IF ( VALID-FG = "N" )
              GO TO B020-ENTER-ELE.


           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO LC-SERVACCT.

           IF NOT ( LC-SERVACCT = "Y" OR "N" )
              PERFORM INVALID-CODE
              GO TO B020-ENTER-ELE.

           PERFORM VALID-COMBINATION.

           IF ( VALID-FG = "N" )
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUBUF TO LC-LOANTYPE.

           IF NOT ( LC-LOANTYPE = "I" OR "P" )
              MOVE "ENTER 'I' OR 'P' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           PERFORM VALID-COMBINATION.

           IF ( VALID-FG = "N" )
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUBUF TO LC-LCARDFG.
           IF NOT ( LC-LCARDFG = "Y" OR "N" )
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUBUF TO LC-PROF1.
           IF NOT ( LC-PROF1 = "C" OR "D" )
              MOVE "ENTER 'C' OR 'D' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUBUF TO LC-PROF2.
           IF NOT ( LC-PROF2 = "R" OR "D" )
              MOVE "ENTER 'R' OR 'D' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDUBUF TO LC-DISPLAY-ERN.
           IF NOT ( LC-DISPLAY-ERN  = "A" OR "T" OR "U" OR "P" OR "I" )
              MOVE "ENTER 'A', 'T', 'U', 'P' OR 'I' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDUBUF TO LC-CALC-HI-CREDIT.
      * REMOVED "W" WCBC 6/22/2023
           IF NOT ( LC-CALC-HI-CREDIT = "I" OR "C" OR "L")
              MOVE "ENTER 'C', 'L', 'I' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           MOVE VDUBUF TO LC-SKIP-TRW.
           IF NOT ( LC-SKIP-TRW = "Y" OR "N" )
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           MOVE VDUBUF TO LC-STOP-ON-DLNO.
           IF NOT ( LC-STOP-ON-DLNO = "Y" OR "N" OR "R")
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-14.
           MOVE VDUBUF TO LC-CON-PDUE-DAYS.
           IF NOT ( LC-CON-PDUE-DAYS = "C" OR "D" )
              MOVE "ENTER 'C' OR 'D' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-15.
           MOVE VDUBUF TO LC-REC-PDUE-DAYS.
           IF NOT ( LC-REC-PDUE-DAYS = "R" OR "D" )
              MOVE "ENTER 'R' OR 'D' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.
      *----------------------------------------------------------------
       F-16.
           MOVE VDUBUF TO LC-APP-REQ-FOR-LOAN.
           IF NOT (LC-APP-REQ-FOR-LOAN = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.

      *---------------------------------------------------------
       F-17.
           MOVE VDUNUM0 TO LC-POOLID.

           IF LC-POOLID = 0
              GO TO B020-CREATE-ROUTINE.

           MOVE "PL"      TO CD-TYPE.
           MOVE LC-POOLID TO CD-PL-POOLID.
           MOVE SPACES    TO CD-PL-FILLER.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-BAD
              MOVE "POOLID IS NOT FOUND IN POOL FILE" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.

       F-18.
           MOVE VDUBUF TO LC-USE-ALT-GL-FEE.
           IF NOT (LC-USE-ALT-GL-FEE = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.

       F-19.
           MOVE VDUNUM0 TO LC-PROGRP-CLASS.
           IF LC-PROGRP-CLASS = 0
              GO TO B020-CREATE-ROUTINE.

           IF LC-SALEFIN = "Y"
            IF LC-PROGRP-CLASS NOT = GP-PROGRP-SF-CLASS(1)
             IF LC-PROGRP-CLASS NOT = GP-PROGRP-SF-CLASS(2)
              IF LC-PROGRP-CLASS NOT = GP-PROGRP-SF-CLASS(3)
               IF LC-PROGRP-CLASS NOT = GP-PROGRP-SF-CLASS(4)
                  MOVE "NOT A VALID GLOBAL PARAMETER PROGRP CLASS"
                     TO MESS
                  PERFORM SEND-MESS
                  GO TO B020-ENTER-ELE.

           IF LC-SALEFIN = "N"
            IF LC-PROGRP-CLASS NOT = GP-PROGRP-LN-CLASS(1)
             IF LC-PROGRP-CLASS NOT = GP-PROGRP-LN-CLASS(2)
              IF LC-PROGRP-CLASS NOT = GP-PROGRP-LN-CLASS(3)
                 MOVE "NOT A VALID GLOBAL PARAMETER PROGRP CLASS"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-20.
           MOVE VDUBUF TO LC-ALLOW-CASH-PROCEEDS.
           IF NOT (LC-ALLOW-CASH-PROCEEDS = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.

       F-21.
           MOVE VDUBUF TO LC-SALES-FORCED-ENTRY.

           IF NOT ( LC-SALES-FORCED-ENTRY = " " )
              MOVE "ENTER ' ' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE
           ELSE
              GO TO B020-CREATE-ROUTINE.

           GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------

       F-99.
           MOVE VDUBUF TO LC-GIFTNET.

           IF NOT ( LC-GIFTNET = "Y" OR "N" )
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           IF ( LC-GIFTNET NOT = "N" ) AND
              ( LC-SALEFIN     = "Y" )
              MOVE "ENTER 'N' FOR SALES FINANCE CLASSES" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.

       CHECK-LOANS.
           PERFORM OPEN-LN3-FILE.
           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN3-WBEG-OWNBR
                                 QLN3-WEND-OWNBR.
           MOVE LC-CODE       TO LN-CLASS
                                 QLN3-WBEG-CLASS
                                 QLN3-WEND-CLASS.
           MOVE 0             TO LN-ACCTNO
                                 QLN3-WBEG-ACCTNO.
           MOVE ALL "9"       TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

           PERFORM READ-LN3-FILE-NEXT.
           IF (IO-FG = 0) AND (LN-OWNBR = LN-PATH-OWNBR)
                          AND (LN-CLASS = LC-CODE)
                          AND (LN-POCD NOT = "**")
              PERFORM LOANS-EXIST.

           PERFORM CLOSE-LN3-FILE.

       END-B020-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       B030-WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-LC1-FILE.

           IF ( NEW-FLAG NOT = 0 )
              GO TO B030-WRITE-RECORD.

           MOVE LC-REC TO LCCH-REC.

           IF ( LCOR-REC = LCCH-REC )
              GO TO END-B030-WRITE-MODULE.

           PERFORM READ-LC1-FILE.
           IF ( IO-FG NOT = 0 )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-B030-WRITE-MODULE.

           IF ( LC-REC NOT = LCOR-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-B030-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE LCCH-REC TO LC-REC.
           PERFORM REWRITE-LC1-FILE.

           GO TO END-B030-WRITE-MODULE.

       B030-WRITE-RECORD.
           PERFORM WRITE-LC1-FILE.
           IF ( IO-FG NOT = 0 )
              PERFORM ALREADY-CREATED.

       END-B030-WRITE-MODULE.
           PERFORM CLOSE-LC1-FILE.

      **************************************
       LOG-MAINTENANCE-CHGS SECTION.
      **************************************
           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LC-CODE   TO WORK-ERRLOG-LC-CODE.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LCOR-DESC NOT = LCCH-DESC
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "CL DESC "              TO WORK-ERRLOG-DESC
             MOVE LCOR-DESC               TO WORK-ERRLOG-ORG-ALP
             MOVE LCCH-DESC               TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-SMALLOAN NOT = LCCH-SMALLOAN
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "SM LOAN "              TO WORK-ERRLOG-DESC
             MOVE LCOR-SMALLOAN           TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-SMALLOAN           TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-SALEFIN NOT = LCCH-SALEFIN
             MOVE "03 "                   TO WORK-ERRLOG-FIELD
             MOVE "SALE FIN"              TO WORK-ERRLOG-DESC
             MOVE LCOR-SALEFIN            TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-SALEFIN            TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-SERVACCT NOT = LCCH-SERVACCT
             MOVE "05 "                   TO WORK-ERRLOG-FIELD
             MOVE "SRV ACCT"              TO WORK-ERRLOG-DESC
             MOVE LCOR-SERVACCT           TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-SERVACCT           TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-LOANTYPE NOT = LCCH-LOANTYPE
             MOVE "06 "                   TO WORK-ERRLOG-FIELD
             MOVE "INT TYPE"              TO WORK-ERRLOG-DESC
             MOVE LCOR-LOANTYPE           TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-LOANTYPE           TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-LCARDFG NOT = LCCH-LCARDFG
             MOVE "07 "                   TO WORK-ERRLOG-FIELD
             MOVE "LED CARD"              TO WORK-ERRLOG-DESC
             MOVE LCOR-LCARDFG            TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-LCARDFG            TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-PROF1 NOT = LCCH-PROF1
             MOVE "08 "                   TO WORK-ERRLOG-FIELD
             MOVE "DEL PRO1"              TO WORK-ERRLOG-DESC
             MOVE LCOR-PROF1              TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-PROF1              TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-PROF2 NOT = LCCH-PROF2
             MOVE "09 "                   TO WORK-ERRLOG-FIELD
             MOVE "DEL PRO2"              TO WORK-ERRLOG-DESC
             MOVE LCOR-PROF2              TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-PROF2              TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-DISPLAY-ERN NOT = LCCH-DISPLAY-ERN
             MOVE "10 "                   TO WORK-ERRLOG-FIELD
             MOVE "DIS EARN"              TO WORK-ERRLOG-DESC
             MOVE LCOR-DISPLAY-ERN        TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-DISPLAY-ERN        TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-CALC-HI-CREDIT NOT = LCCH-CALC-HI-CREDIT
             MOVE "11 "                   TO WORK-ERRLOG-FIELD
             MOVE "HIGHCRED"              TO WORK-ERRLOG-DESC
             MOVE LCOR-CALC-HI-CREDIT     TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-CALC-HI-CREDIT     TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-SKIP-TRW NOT = LCCH-SKIP-TRW
             MOVE "12 "                   TO WORK-ERRLOG-FIELD
             MOVE "SKIP CB "              TO WORK-ERRLOG-DESC
             MOVE LCOR-SKIP-TRW           TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-SKIP-TRW           TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-STOP-ON-DLNO NOT = LCCH-STOP-ON-DLNO
             MOVE "13 "                   TO WORK-ERRLOG-FIELD
             MOVE "STP DLNO"              TO WORK-ERRLOG-DESC
             MOVE LCOR-STOP-ON-DLNO       TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-STOP-ON-DLNO       TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-CON-PDUE-DAYS NOT = LCCH-CON-PDUE-DAYS
             MOVE "14 "                   TO WORK-ERRLOG-FIELD
             MOVE "CON DAYS"              TO WORK-ERRLOG-DESC
             MOVE LCOR-CON-PDUE-DAYS      TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-CON-PDUE-DAYS      TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-REC-PDUE-DAYS NOT = LCCH-REC-PDUE-DAYS
             MOVE "15 "                   TO WORK-ERRLOG-FIELD
             MOVE "REC DAYS"              TO WORK-ERRLOG-DESC
             MOVE LCOR-REC-PDUE-DAYS      TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-REC-PDUE-DAYS      TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-APP-REQ-FOR-LOAN NOT = LCCH-APP-REQ-FOR-LOAN
             MOVE "16 "                   TO WORK-ERRLOG-FIELD
             MOVE "APP REQ "              TO WORK-ERRLOG-DESC
             MOVE LCOR-APP-REQ-FOR-LOAN   TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-APP-REQ-FOR-LOAN   TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-POOLID NOT = LCCH-POOLID
             MOVE "17 "                   TO WORK-ERRLOG-FIELD
             MOVE "POOL ID "              TO WORK-ERRLOG-DESC
             MOVE LCOR-POOLID             TO WORK-ERRLOG-ORG-N0
             MOVE LCCH-POOLID             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-USE-ALT-GL-FEE NOT = LCCH-USE-ALT-GL-FEE
             MOVE "18 "                   TO WORK-ERRLOG-FIELD
             MOVE "ALT FEE "              TO WORK-ERRLOG-DESC
             MOVE LCOR-USE-ALT-GL-FEE     TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-USE-ALT-GL-FEE     TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-PROGRP-CLASS NOT = LCCH-PROGRP-CLASS
             MOVE "19 "                   TO WORK-ERRLOG-FIELD
             MOVE "PRO CLAS"              TO WORK-ERRLOG-DESC
             MOVE LCOR-PROGRP-CLASS       TO WORK-ERRLOG-ORG-N2
             MOVE LCCH-PROGRP-CLASS       TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-ALLOW-CASH-PROCEEDS NOT = LCCH-ALLOW-CASH-PROCEEDS
             MOVE "20 "                   TO WORK-ERRLOG-FIELD
             MOVE "CASH PRO"              TO WORK-ERRLOG-DESC
             MOVE LCOR-ALLOW-CASH-PROCEEDS       TO WORK-ERRLOG-ORG-N2
             MOVE LCCH-ALLOW-CASH-PROCEEDS       TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-SALES-FORCED-ENTRY NOT = LCCH-SALES-FORCED-ENTRY
             MOVE "21 "                   TO WORK-ERRLOG-FIELD
             MOVE "FORCE EN"              TO WORK-ERRLOG-DESC
             MOVE LCOR-SALES-FORCED-ENTRY TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-SALES-FORCED-ENTRY TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

           IF LCOR-GIFTNET NOT = LCCH-GIFTNET
             MOVE "99 "                   TO WORK-ERRLOG-FIELD
             MOVE "GIFT NET"              TO WORK-ERRLOG-DESC
             MOVE LCOR-GIFTNET            TO WORK-ERRLOG-ORG-ALP2
             MOVE LCCH-GIFTNET            TO WORK-ERRLOG-CHG-ALP2
             PERFORM LOG-FIELD-CHANGE.

      **********************************************
       LOG-FIELD-CHANGE SECTION.
      **********************************************
           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ******************************************************************
       B040-DELETE-MODULE SECTION.
      ******************************************************************
           IF ( NEW-FLAG NOT = 0 )
              GO TO END-B040-DELETE-MODULE.

           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           IF ( IO-FG NOT = 0 )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-B040-DELETE-MODULE.

           IF ( LC-REC NOT = LCOR-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-B040-DELETE-MODULE.

           PERFORM OPEN-LN3-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN3-WBEG-OWNBR
                                 QLN3-WEND-OWNBR.
           MOVE LC-CODE       TO LN-CLASS
                                 QLN3-WBEG-CLASS
                                 QLN3-WEND-CLASS.
           MOVE 0             TO LN-ACCTNO
                                 QLN3-WBEG-ACCTNO.
           MOVE ALL "9"       TO QLN3-WEND-ACCTNO.
           PERFORM START-LN3-FILE.

           PERFORM READ-LN3-FILE-NEXT.
           IF (IO-FG = 0) AND (LN-OWNBR = LN-PATH-OWNBR)
                          AND (LN-CLASS = LC-CODE)
                          AND (LN-POCD NOT = "**")
               PERFORM NO-DELETE
           ELSE
               PERFORM DELETE-LC1-FILE
               MOVE "C" TO ERRLOGW-ACTION
               MOVE " " TO ERRLOGW-KEY
               STRING LC1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY
               MOVE FORM-PATHNAME TO ERRLOGW-PROGX
               MOVE "CL/ERRLOG" TO FORM-NAM
               CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA
               CANCEL FORM-PROGX.
               

           PERFORM CLOSE-LN3-FILE.

       END-B040-DELETE-MODULE.
           PERFORM CLOSE-LC1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1                     TO NEW-FLAG.
           MOVE "S  A000NEW."         TO VDU.
           MOVE "*** NEW RECORD ***"  TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LC-FILE.

           MOVE HOLD-LC1-KEY  TO LC-CODE.
           MOVE 0         TO ELE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES                 TO DISPLAY-BUF.
           MOVE LC-DESC                TO F01.
           MOVE LC-SMALLOAN            TO F02.
           MOVE LC-SALEFIN             TO F03.
           MOVE SPACES                 TO F04.
           MOVE LC-SERVACCT            TO F05.
           MOVE LC-LOANTYPE            TO F06.
           MOVE LC-LCARDFG             TO F07.
           MOVE LC-PROF1               TO F08.
           MOVE LC-PROF2               TO F09.
           MOVE LC-DISPLAY-ERN         TO F10.
           MOVE LC-CALC-HI-CREDIT      TO F11.
           MOVE LC-SKIP-TRW            TO F12.
           MOVE LC-STOP-ON-DLNO        TO F13.
           MOVE LC-CON-PDUE-DAYS       TO F14.
           MOVE LC-REC-PDUE-DAYS       TO F15.
           MOVE LC-APP-REQ-FOR-LOAN    TO F16.
           MOVE LC-POOLID              TO F17.
           MOVE LC-USE-ALT-GL-FEE      TO F18.
           MOVE LC-PROGRP-CLASS        TO F19.
           MOVE LC-ALLOW-CASH-PROCEEDS TO F20.
           MOVE LC-SALES-FORCED-ENTRY  TO F21.
           MOVE LC-GIFTNET             TO F99.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

      ******************************************************************
       VALID-COMBINATION SECTION.
      ******************************************************************

           MOVE SPACES TO VALID-FG.

      * CHECK: SMALL LOAN
           IF ( LC-SMALLOAN NOT = "Y" )
              GO TO TEST-VALID-COMB-SERVACCT.

      * CHECK: SALE FINANCE FOR SMALL LOAN

           IF ( LC-SALEFIN NOT = "Y" )
              GO TO TEST-VALID-COMB-SERVACCT.

           IF ( EXT-CONAME-USER-LOC NOT NUMERIC )
              MOVE "N" TO VALID-FG
              GO TO TEST-VALID-COMB-SERVACCT.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-FG NOT = ZEROES )
              MOVE "N" TO VALID-FG
              GO TO TEST-VALID-COMB-SERVACCT.

           IF ( BR-SALES-SMALL NOT = "Y" )
              MOVE "SMALL LOANS MUST BE DIRECT LOANS" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO VALID-FG.

      * CHECK: SERVICE ACCOUNTS
       TEST-VALID-COMB-SERVACCT.

           IF ( LC-SERVACCT = "Y" ) AND
              ( LC-SALEFIN  = "N" )
              MOVE " SERVICE ACCOUNTS MUST BE SALES FINANCE" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO VALID-FG.

       EXIT-VALID-COMBINATION.
           EXIT.


      ******************************************************************
      *     C O P Y   L I B R A R Y    S E C T I O N S
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPLCCL.CPY".

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO LCMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LCMAIN-SYSDATE.
      *    DISPLAY OMITTED ERASE SCREEN.
      * USING THE LINE 1 COL 1 WORKS MUCH MORE CONSISTENT!
      * THE OMITTED LINE WOULD NOT CLEAR ON A F1 FROM THE BOTTOM BUT WOULD
      * CLEAR WHEN PART WAY THRU A NEW RECORD. DIDNT MAKE SENSE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LCMAIN-SCREEN UPON LCMAIN-WINDOW-HANDLE.
           MOVE LCMAIN-FORM-FIELDS TO WR-BUF.
           MOVE LCMAIN-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM. 

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE LCMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE LCMAIN-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LCMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A400MSEL."  TO VDU.
           MOVE MESS            TO VDUBUF.
           PERFORM SCREENIO.
       INVALID-CODE.
           MOVE " ENTER 'Y' OR 'N'" TO MESS.
           PERFORM SEND-MESS.
       LOANS-EXIST.
           MOVE " LOANS EXIST FOR THIS CLASS - CANNOT BE CHANGED!"
                TO MESS.
           PERFORM SEND-MESS.
           MOVE "CLR" TO IO-TYPE.
       NO-DELETE.
           MOVE " LOANS EXIST FOR THIS CLASS - CANNOT DELETE!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPLC1IN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH   TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LCMAIN-SCREEN.
           DESTROY LCMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.

