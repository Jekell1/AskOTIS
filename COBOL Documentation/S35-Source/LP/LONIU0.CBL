       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONIU0.
       DATE-WRITTEN.    050286.
      ******************************************************************
      *
      *                 (LP)
      *  DESCRIPTION:   SALES FINANCE LOAN
      *                 INQUIRE/MODIFY PAGE 3
      *
      *=================================================================
      * REV:
      * 032289 JTG REMOVED REF TO LN-EXTDAYS
      * 080390 BAH REPLACED LN-LNAMT WITH LN-SUM-LNAMT FOR
      *            ADDONS
      * 032593 JTG REVISED TO PASS BR-REC AND GB-REC
      * 051293 JTG REVISED FOR LN-LCPDTH-DATE (MMDDYY)
      * 091093 JTG ADDED LOGIC FOR LN-PLTYPE
      * KEC 112596 CHANGED LN-POCD FOR RESCISSION.
      * MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      *  CS 991018 ADD TEST OF BULK-CONVERSION-FG TO DETERMINE IF TO
      *            DISPLAY BULK PURCHASE OR CONVERSION
      * MJD 000912 ADDED LN-LBOX  (MFC PR#227)
      * BLM 161227 CHANGED ORIG & CHANGED RECS FROM 1800 TO LN-SIZE
      *
      * KEC 2017.0526 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *
      * BAH 170612 ACTIVATE 8 DIGIT DATES, PD0006
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONIU0.CBL S34 05/21/21-13:37:23 >".
      **********************************
      *       F I L E   P A T H S      *
      **********************************
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

       01  ONE             PIC S9.
       01  ELE             PIC 9(6)    VALUE 0.
       01  ELE-OVER-20     PIC 9(6)    VALUE 0.
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9       VALUE 0.
       01  FIRST-ELE       PIC 99 VALUE 0.
       01  ORIGINAL-LN-REC    PIC X(LN-SIZE).
       01  CHANGED-LN-REC     PIC X(LN-SIZE).

       01  SUB             PIC 9(6) COMP-3.
       01  INSPECT-W       PIC X(86).
       01  ADD-ON          PIC X(25).

       01  ALP-BW.
           03  ALP-BW123   PIC X(3).
           03  ALP-HYPHEN1 PIC X.
           03  ALP-BW45    PIC X(2).
           03  ALP-HYPHEN2 PIC X.
           03  ALP-BW6789  PIC X(4).
       01  NUM-BW          PIC 9(9).
       01  NUM-BWX REDEFINES NUM-BW.
           03  NUM-BW123   PIC X(3).
           03  NUM-BW45    PIC X(2).
           03  NUM-BW6789  PIC X(4).

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BUF1        VALUE " ".
           03  D-LNNO          PIC 9(8).
           03  D-P             PIC X(2).
           03  D-PLTYPE        PIC X(2).
           03  D-J             PIC X(3).
           03  D-LCNO          PIC 99.
           03  D-LC-DESC       PIC X(15).
           03  D-LNAMT         PIC ZZZZZ9.99.
           03  D-PROCEEDS      PIC ZZZZZ9.99.
           03  D-010-040       OCCURS 4.
               05  D-010       PIC X(11).
               05  D-011       PIC X(33).
               05  D-012       PIC X(1).
           03  D-050           PIC Z(5)9.99-  BLANK ZERO.
           03  D-060           PIC Z(5)9.99-  BLANK ZERO.
           03  D-070           PIC ZZ.
           03  D-080           PIC ZZ.
           03  D-090           PIC Z(3)-.
           03  D-100           PIC Z(6).
           03  D-110           PIC X.
           03  D-120           PIC Z(5)9.99  BLANK ZERO.
           03  D-130           PIC Z(5)9.99  BLANK ZERO.
           03  D-140           PIC Z(5)9.99  BLANK ZERO.
           03  D-150           PIC Z(3)9.99  BLANK ZERO.
           03  D-160           PIC X.
           03  D-170           PIC ZZZ9      BLANK ZERO.
           03  D-190           PIC Z(6).99   BLANK ZERO.
           03  D-200           PIC ZZ/ZZ/ZZ  BLANK ZERO.
           03  D-210           PIC X.
           03  D-211           PIC X.
           03  D-220           PIC ZZ9.999 BLANK ZERO.
           03  D-230           PIC ZZ/ZZ/ZZ.
           03  D-240           PIC ZZ9.999 BLANK ZERO.
           03  D-250           PIC ZZ/ZZ/ZZ.
           03  D-260           PIC X.
           03  D-270           PIC ZZ9.999 BLANK ZERO.
           03  D-280           PIC ZZ9.999 BLANK ZERO.
           03  D-290           PIC X(10).

       01  DISPLAY-LIST1.
           03  FILLER          PIC X(36)  VALUE
           "LNNO LNSQ PJ LNKEY LCDESC PRIN DLCK ".
           03  FILLER          PIC X(52)  VALUE
           "010 011 012 020 021 022 030 031 032 040 041 042 050 ".
           03  FILLER          PIC X(36)  VALUE
           "060 070 080 090 100 110 120 130 140 ".
           03  FILLER          PIC X(36)  VALUE
           "150 160 170 190 200 210 211 220 230 ".
           03  FILLER          PIC X(24)  VALUE
           "240 250 260 270 280 290.".

      ********************************
      *   E L E M E N T   S P E C S  *
      ********************************
       01  MAX             PIC 99   VALUE 29.
       01  SPEC-TABLE.
      * 1  LN-SSNO(1)
           03  FILLER      PIC X(8) VALUE "E  A110N".
      * 2  LN-SSNO(2)
           03  FILLER      PIC X(8) VALUE "E  A110N".
      * 3  LN-SSNO(3)
           03  FILLER      PIC X(8) VALUE "E  A110N".
      * 4  LN-SSNO(4)
           03  FILLER      PIC X(8) VALUE "E  A110N".
      * 5  LN-TOTERN
           03  FILLER      PIC X(8) VALUE "ENNS062 ".
      * 6  LN-LCWAIVERS
           03  FILLER      PIC X(8) VALUE "ENNS062 ".
      * 7  LN-YTDNODEF
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 8  LN-DELCNT2
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 9  LN-CRSCORE
           03  FILLER      PIC X(8) VALUE "ENNS030 ".
      * 10 LN-CREDITLIM
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      * 11 LN-CONFIDENTCD
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 12 LN-TOTDEF
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 13 LN-TOTINT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 14 LN-TOTLCHG
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 15 LN-LCPARTIALS
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 16 UNUSED
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 17 LN-STMT-COUNTER
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      * 18  FILLER
           03  FILLER      PIC X(8) VALUE "ENNN020N".
      * 19 LN-ORIGPURAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 20 LN-LCPDTH-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 21 LN-ALLOTCD
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 22 LN-ACCRATE
           03  FILLER      PIC X(8) VALUE "EN N033 ".
      * 23 LN-ACCELDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 24 LN-REDURATE
           03  FILLER      PIC X(8) VALUE "EN N033 ".
      * 25 LN-REDUDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 26 LN-STATUS
           03  FILLER      PIC X(8) VALUE "ENRA010 ".
      * 27 LN-SMPRATE
           03  FILLER      PIC X(8) VALUE "EN N033 ".
      * 28 LN-EFFRATE
           03  FILLER      PIC X(8) VALUE "EN N033 ".
      * 29 LN-STATFLAGS
           03  FILLER      PIC X(8) VALUE "E  A100 ".


       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 29 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPMNUIW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONIU_DEF.CPY".
       COPY "LIBLP/LPCMNU_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBLP/LONIU_WKS.CPY".
       COPY "LIBLP/LPCMNU_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLONIW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONIU_SCN.CPY".
       COPY "LIBLP/LPCMNU_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF.

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONIU" TO VDU-FORM-NAME.

           MOVE USER-PID TO TEMP-PID.
           MOVE " " TO ERRCD.

      D    STOP MESS.
           IF ROUTE-CD = "1"
              MOVE " " TO ROUTE-CD
              MOVE 2 TO NEW-FLAG
              GO TO ENTER-LOAN.

           IF NEW-FLAG = 1
              PERFORM DISPLAY-FIELDS
              GO TO ENTER-LOAN.

       MAIN-MODULE.
           PERFORM FORM-RESET.
           PERFORM DISPLAY-BULK-CONVERSION.
           MOVE " " TO MESS.
           PERFORM MSG-LINE.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
       ENTER-LOAN.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           GO TO MAIN-MODULE.

      ************************************
       ENTER-KEYS SECTION.
      ************************************
       LOAN-GET-NUMBER.
           MOVE 0 TO NEW-FLAG ELE.
           MOVE SPACES TO IO-TYPE.
           IF LOAN-KEY NOT = 0
              MOVE LOAN-KEY TO LN-ACCTNO
              MOVE 0 TO LOAN-KEY
              MOVE "SN N060LNNO." TO VDU
              MOVE LN-ACCTNO TO VDUNUM0
              PERFORM SCREENIO
              GO TO START-LN-LOOKUP.
           MOVE "F2-MENU" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N060LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "12"
              GO TO MENU-KEY.
           IF VDUSTATUS NOT = "00"
              GO TO LOAN-GET-NUMBER.
           IF VDUNUM0 = 0
              GO TO LOAN-GET-NUMBER.
           MOVE VDUNUM0 TO LN-ACCTNO.
       START-LN-LOOKUP.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO LOAN-GET-NUMBER.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO LOAN-GET-NUMBER.

           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS IN RESCISSION" TO MESS
              PERFORM SEND-MESS
              GO TO LOAN-GET-NUMBER.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO LOAN-GET-NUMBER.

      *---{CS}----
           IF BULK-CONVERSION-FG = "B"
              IF LN-CONVERCD NOT = "B"
                 MOVE "ACCOUNT NOT INPUT AS BULK PURCHASE ACCOUNT"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO LOAN-GET-NUMBER.

           IF BULK-CONVERSION-FG = "C"
              IF LN-CONVERCD NOT = "Y"
                 MOVE "ACCOUNT NOT INPUT AS CONVERSION ACCOUNT"
                                               TO MESS
                 PERFORM SEND-MESS
                 GO TO LOAN-GET-NUMBER.

           MOVE LN-REC TO ORIGINAL-LN-REC.
           PERFORM DISPLAY-FIELDS.
           IF LC-SALEFIN NOT = "Y"
              PERFORM NOT-SALES-FINANCE
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           MOVE 2 TO NEW-FLAG.
           GO TO EXIT-ENTER-KEYS.
       MENU-KEY.
           IF LN-ACCTNO NUMERIC
              MOVE LN-ACCTNO TO LOAN-KEY.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONIU-SCREEN.

           PERFORM MENU-SELECTION.
           MOVE 0 TO ELE.
           IF MENU-FG = 99
              GO TO LOAN-GET-NUMBER.
           MOVE "END" TO IO-TYPE.
       EXIT-ENTER-KEYS.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE "S  A000MSEL." TO VDU.
           PERFORM SCREENIO.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE FIRST-ELE TO ELE.
           MOVE 0 TO FIRST-ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           MOVE 2 TO NEW-FLAG.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
       MODIFY-ROUTINE.
           ADD MAX 1 GIVING ELE.
           MOVE "F2-MENU,F4-MEMO,FIELD#,P1,P2,U-UPDATE" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "12"
              IF LN-ACCTNO NUMERIC
                 MOVE LN-ACCTNO TO LOAN-KEY.
           IF VDUSTATUS = "12"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LONIU-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "END" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "18"
              PERFORM MEMO-ENTRY
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "    P1"
              MOVE LN-ACCTNO TO LOAN-KEY
              PERFORM WRITE-MODULE
              MOVE "J0J" TO NEW-PROG-FORM
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "    P2"
              MOVE LN-ACCTNO TO LOAN-KEY
              PERFORM WRITE-MODULE
              MOVE "K0K" TO NEW-PROG-FORM
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "     U"
              PERFORM WRITE-MODULE
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL " " BY "0".
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF LN-CURBAL NOT = 0
              OR LN-INTBAL NOT = 0
              OR LN-LCBAL NOT = 0
              OR LN-OTHBAL NOT = 0
              MOVE "CURRENT, INT, LCHG, AND OTH BALS MUST ALL BE O.OO"
                TO MESS
              PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.
      *    IF ELE < 5
      *       MOVE "BORROWER #, F6-SCAN BY NAME" TO MESS
      *       PERFORM SEND-LEGEND.
      *    IF ELE = 5
      *       PERFORM RESET-LEGEND.

           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
       TEST-F2.
           IF VDUSTATUS NOT = "12"
              GO TO TEST-F4.
           IF LN-ACCTNO NUMERIC
              MOVE LN-ACCTNO TO LOAN-KEY.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONIU-SCREEN.

           PERFORM MENU-SELECTION.
           IF MENU-FG = 99
              GO TO ENTER-ELE.
           MOVE "END" TO IO-TYPE.
           GO TO END-ROUTINE.
       TEST-F4.
           IF VDUSTATUS NOT = "1U"
              GO TO TEST-F5.
      *    IF ELE = 6
      *       MOVE " " TO CHFG(5).
           IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS NOT = "1D"
              GO TO TEST-F6.
           MOVE 1 TO ONE.
           GO TO GOTO-FIELD.
       TEST-F6.
           IF VDUSTATUS NOT = "1<"
              GO TO TEST-00.
      *    IF ELE < 5
      *       PERFORM BORROWER-SCAN
      *       IF IO-FG = 0
      *          GO TO GOTO-FIELD
      *       ELSE
      *          GO TO ENTER-ELE.
       TEST-00.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
       GOTO-FIELD.
           MOVE 1 TO ONE.

           IF ELE > 20
              SUBTRACT 20 FROM ELE GIVING ELE-OVER-20
              GO TO OVER-20.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
       OVER-20.
           GO TO F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29
                 DEPENDING ON ELE-OVER-20.
           GO TO END-ENTRY-MODULE.
      *************************************
      *    S E T   E L E M E N T S        *
      *************************************
       F-01.
      *    PERFORM BORROWER-ENTRY.
      *    IF IO-FG = 9
      *       GO TO ENTER-ELE.
      *    GO TO CREATE-ROUTINE.
       F-02.
      *    PERFORM BORROWER-ENTRY.
      *    IF IO-FG = 9
      *       GO TO ENTER-ELE.
      *    GO TO CREATE-ROUTINE.
       F-03.
      *    PERFORM BORROWER-ENTRY.
      *    IF IO-FG = 9
      *       GO TO ENTER-ELE.
      *    GO TO CREATE-ROUTINE.
       F-04.
      *    PERFORM BORROWER-ENTRY.
      *    IF IO-FG = 9
      *       GO TO ENTER-ELE.
      *    PERFORM BW-CHECK-ENDORSER.
      *    GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO LN-TOTERN.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM2 TO LN-LCWAIVERS.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM0 TO LN-YTDNODEF.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM0 TO LN-DELCNT2.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM0 TO LN-CRSCORE.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUNUM0 TO LN-CREDITLIM.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUBUF TO LN-CONFIDENTCD.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM2 TO LN-TOTDEF.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUNUM2 TO LN-TOTINT.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUNUM2 TO LN-TOTLCHG.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUNUM2 TO LN-LCPARTIALS.
           GO TO CREATE-ROUTINE.
       F-16.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUNUM0 TO LN-STMT-COUNTER.
           GO TO CREATE-ROUTINE.
       F-18.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM2 TO LN-ORIGPURAMT.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDU-DATE-YYYYMMDD TO LN-LCPDTH-DATE.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUBUF TO LN-ALLOTCD.
           IF LN-ALLOTCD NOT = "Y" AND
                               NOT = "N"
              MOVE "ENTER: Y OR N" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       F-211.
           IF LN-ALLOTCD = "Y"
              MOVE "S  A010211." TO VDU
              MOVE "N" TO VDUBUF LN-LBOX
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
           MOVE "E  A010211." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT ENTER-KEY GO TO F-211.
           MOVE VDUBUF TO LN-LBOX.
           IF NOT (LN-LBOX = "Y" OR "N")
              MOVE "ENTER: Y OR N" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF LN-LBOX = "Y"
              MOVE "S  A010210." TO VDU
              MOVE "N" TO VDUBUF LN-ALLOTCD
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUNUM3 TO LN-ACCRATE.
           IF LN-ACCRATE = 999.999
              MOVE "RATE MUST BE LESS THAN 999.999" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDU-DATE-YYYYMMDD TO LN-ACCELDATE.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUNUM3 TO LN-REDURATE.
           IF LN-REDURATE > 999.996
              MOVE "RATE MUST BE LESS THAN 999.997" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDU-DATE-YYYYMMDD TO LN-REDUDATE.
           GO TO CREATE-ROUTINE.
       F-26.
           MOVE VDUBUF TO LN-STATUSFG.
           IF LN-STATUSFG NOT = " " AND
                          NOT = "J" AND
                          NOT = "A" AND
                          NOT = "R"
              MOVE " STATUS MUST BE J, A, R, OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-27.
           MOVE VDUNUM3 TO LN-SMPRATE.
           GO TO CREATE-ROUTINE.
       F-28.
           MOVE VDUNUM3 TO LN-EFFRATE.
           GO TO CREATE-ROUTINE.
       F-29.
           MOVE VDUALPHA TO LN-STATFLAGS.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      *********************************
       MEMO-ENTRY SECTION.
      *********************************
           PERFORM SEND-MEMO.
       ENTER-MEMO.
           MOVE "E  A300M2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MEMO.
           MOVE VDUBUF TO LN-MESSAGE.
           IF LN-MESSAGE = " "
              MOVE "M1 M2." TO MESS
              PERFORM RESET-MESS.

      *******************************
       WRITE-MODULE SECTION.
      *******************************
           PERFORM OPEN-LN1-FILE.
           MOVE LN-REC TO CHANGED-LN-REC.
           IF ORIGINAL-LN-REC = CHANGED-LN-REC
              GO TO END-WRITE-MODULE.
           MOVE "UPDATING LOAN MASTER.........." TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF LN-REC NOT = ORIGINAL-LN-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-LN-REC TO LN-REC.
           PERFORM REWRITE-LN1-FILE.
       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE " " TO DISPLAY-BUF1.
           MOVE LN-ACCTNO TO D-LNNO.
           IF LN-PLCD = "P"
              MOVE "PL" TO D-P
              IF LN-PLTYPE NOT = "P"
                 MOVE LN-PLTYPE TO D-PLTYPE.
           IF LN-JDDATE NOT = 0
              MOVE "JDG" TO D-J.
           MOVE LN-CLASS TO LC-CODE D-LCNO.
           PERFORM GET-LC.
           MOVE LC-DESC TO D-LC-DESC.
           MOVE LN-SUM-LNAMT TO D-LNAMT.
           MOVE LN-PROCEEDS TO D-PROCEEDS.

           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO SUB.
       DISP-NEXT-BW.
           MOVE LN-SSNO(SUB) TO BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF BW-SSNO NOT = 0
              PERFORM GET-BW1
              MOVE ALP-BW TO D-010(SUB)
              MOVE INSPECT-W TO D-011(SUB)
              MOVE LN-MAKERCD(SUB) TO D-012(SUB).
           IF SUB < 4
              ADD 1 TO SUB
              GO TO DISP-NEXT-BW.
           PERFORM CLOSE-BW1-FILE.

           MOVE LN-TOTERN TO D-050.
           MOVE LN-LCWAIVERS TO D-060.
           MOVE LN-YTDNODEF TO D-070.
           MOVE LN-DELCNT2 TO D-080.
           MOVE LN-CRSCORE TO D-090.
           MOVE LN-CREDITLIM TO D-100.
           MOVE LN-CONFIDENTCD TO D-110.
           MOVE LN-TOTDEF TO D-120.
           MOVE LN-TOTINT TO D-130.
           MOVE LN-TOTLCHG TO D-140.
           MOVE LN-LCPARTIALS TO D-150.
           MOVE SPACES TO D-160.
           MOVE LN-STMT-COUNTER TO D-170.
           MOVE LN-ORIGPURAMT TO D-190.
           MOVE LN-LCPDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-200.
           MOVE LN-ALLOTCD TO D-210.
           MOVE LN-LBOX TO D-211.
           MOVE LN-ACCRATE TO D-220.
           MOVE LN-ACCELDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-230.
           MOVE LN-REDURATE TO D-240.
           MOVE LN-REDUDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-250.
           MOVE LN-STATUSFG TO D-260.
           MOVE LN-SMPRATE TO D-270.
           MOVE LN-EFFRATE TO D-280.
           MOVE LN-STATFLAGS TO D-290.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF LN-MESSAGE NOT = " "
              PERFORM SEND-MEMO
              PERFORM ALARM.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPMNUI.CPY".
           IF LN-ACCTNO NUMERIC AND MENU-FG NOT = 99
              MOVE LN-ACCTNO TO LOAN-KEY.

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           MOVE LPLONI-SYSDATE TO LONIU-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONIU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONIU-FORM-FIELDS TO WR-BUF.
           MOVE LONIU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE LONIU-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONIU-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONIU_EVA.CPY".
              COPY "LIBLP/LPCMNU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 9
              MOVE "INVALID" TO LC-DESC.
       GET-BW1.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE " " TO BW-FNAME
              MOVE "NOT FOUND" TO BW-LNAME.
           PERFORM APPEND-BW-NAMES.
       APPEND-BW-NAMES.
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           PERFORM ADD-IT.
       ADD-IT.
           INSPECT INSPECT-W REPLACING FIRST "  " BY " +".
           INSPECT INSPECT-W REPLACING FIRST
              "+                        " BY ADD-ON.
       ADD-IT2.
           PERFORM ADD-IT.
           MOVE "/" TO ADD-ON.
           PERFORM ADD-IT.
       DISPLAY-BULK-CONVERSION.
      *-{CS}----
           MOVE "S  A100TITLE." TO VDU.
           IF BULK-CONVERSION-FG = "B"
              MOVE "BULK PURCH" TO VDUBUF
           ELSE
              MOVE "CONVERSION" TO VDUBUF.
           PERFORM SCREENIO.

       NUMBW-TO-ALPBW.
           MOVE " " TO ALP-BW.
           IF BW-SSNO NOT = 0
              MOVE BW-SSNO TO NUM-BW
              MOVE NUM-BW123 TO ALP-BW123
              MOVE NUM-BW45 TO ALP-BW45
              MOVE NUM-BW6789 TO ALP-BW6789
              MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       SEND-MEMO.
           MOVE "S  A060M1." TO VDU.
           MOVE "MEMO: " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A300M2." TO VDU.
           MOVE LN-MESSAGE TO VDUBUF.
           PERFORM SCREENIO.
       NOT-FOUND.
           MOVE "NOT FOUND" TO MESS.
           PERFORM SEND-MESS.
       NOT-SALES-FINANCE.
           MOVE "INVALID FOR SALES FINANCE" TO MESS.
           PERFORM SEND-MESS.
       MSG-LINE.
           MOVE MESS TO TMMSG-MESS.
           PERFORM CALL-TMMSG.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       RESET-LEGEND.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONIU-SCREEN.
           EXIT PROGRAM.
