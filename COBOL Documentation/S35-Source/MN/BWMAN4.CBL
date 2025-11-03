       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       BWMAN4.
       DATE-WRITTEN.    05/19/03.
      **************************************************************************
      *  DESCRIPTION:   BORROWER MASTER FILE MAINTENANCE  PAGE 4
      *
      * REV:
      *  BAH 030510 NEW, ADDED ALTERANTE ADDRESS TO BX-FILE, BORROWER
      *             TRAILER, REGACC PR# 27
      *  MJD 070110 ADDED NEW BORROWER TRAILER FIELDS   REGENCT PR#2856
      *  AMM 070329 CREATED A LOG FIELD CHANGE ROUTINE
      *  MJD 070827 ADDED CODE SCAN FOR KYC IDTYPE               REGENCY PR#3032
      * KEC 2016.0325 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      *  CS 170413 ACTIVATE 8 DIGIT DATES, BX-KYC-ISSDATE,EXPDATE,LAST-VERDATE
      *            ADDED DATER/DATERW
      * BAH 190129 SPLIT CDFILE
      * BLM 210920 MOVE BW-OWNBR TO BX-BRNO, BX RECORDS WERE GETTING WRITTEN
      *            WITH GARBAGE OR SPACES IN BX-BRNO
      * BLM 210924 FIX CAST ERROR IF ALPHA ENTERED IN SSNO SCAN
      * AMM 220105 ADDED BW-OPT-IN-EMAIL/TEXT FIELDS
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/BWMAN4.CBL S35 02/23/24-10:20:52 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BX.CPY".
       COPY "LIBLP/LP01BX_SQL.CPY".
       COPY "LIBLP/LPWSBX.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

      *=================================================================
      *   BWCH-REC FROM 'REPLACING LEADING' ON BW-REC. [CHANGED-BW-REC]
      *   BWOR-REC FROM 'REPLACING LEADING' ON BW-REC. [ORIGINAL-BW-REC]
      *   BXCH-REC FROM 'REPLACING LEADING' ON BW-REC. [CHANGED-BX-REC]
      *   BXOR-REC FROM 'REPLACING LEADING' ON BW-REC. [ORIGINAL-BX-REC]
      *=================================================================
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "BWOR".
       COPY "LIBLP/LP01BX.CPY" REPLACING LEADING "BX" BY "BXOR".
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "BWCH".
       COPY "LIBLP/LP01BX.CPY" REPLACING LEADING "BX" BY "BXCH".

      ******************************************************************
      *
      *    L M C - W O R K E R S
      *
      *=================================================================
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-BRNO                PIC 9(04).
           03  FILLER                  PIC X(01).
           03  LMC-FIELD               PIC X(03).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(09).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(16).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-ORG-DATE            REDEFINES LMC-ORG-TEXT
                                       PIC 99/99/99.
           03  LMC-ORG-DLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-ORG-GLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-ORG-PHONE           REDEFINES LMC-ORG-TEXT
                                       PIC 999/999/9999.
           03  LMC-ORG-SSNO            REDEFINES LMC-ORG-TEXT
                                       PIC 999/99/9999.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(16).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-CHG-DATE            REDEFINES LMC-CHG-TEXT
                                       PIC 99/99/99.
           03  LMC-CHG-DLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-CHG-GLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-CHG-PHONE           REDEFINES LMC-CHG-TEXT
                                       PIC 999/999/9999.
           03  LMC-CHG-SSNO            REDEFINES LMC-CHG-TEXT
                                       PIC 999/99/9999.

      ******************************************************************

       01  IX1             PIC 99      VALUE 0.
       01  IX2             PIC 99      VALUE 0.

       01  IDTYPE-CODE     PIC X(2).
       01  IDTYPE-DESC     PIC X(35).
       01  IDTYPE-VALID-YN PIC X.
           88  IDTYPE-VALID VALUE 'Y'.

       01  TYP             PIC 9       VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
           03  CHNO-X      REDEFINES CHNO
                           PIC X(2).

       01  FORM-NUM1       REDEFINES ACTION.
           03  FORM-KEY1   PIC X(4).
           03  FORM-P1     PIC X.
           03  FORM-SEL1   PIC 9.

       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9       VALUE 0.

       01  HOLD-KEY        PIC 9(9).
       01  SOCSEC-KEY      PIC 9(9).

       01  BW-ALP-TABLE.
           03  BW-ALP-TBL  OCCURS 70 TIMES PIC X.

       01  BWMAIN-LINK       VALUE SPACES.
           03  SAVEKEY       PIC X(9).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  BWMAIN-WINDOW-HANDLE PIC X(10).


      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  D-NAME      PIC X(34).
           03  F01                 PIC X(24).
           03  F02                 PIC X(24).
           03  F03                 PIC X(15).
           03  F04                 PIC X(2).
           03  F05                 PIC X(10).
           03  F06                 PIC X(24).
           03  F07                 PIC X(24).
           03  F08                 PIC X(15).
           03  F09                 PIC X(2).
           03  F10                 PIC X(10).
           03  F11                 PIC X(2).
           03  F12                 PIC X(2).
           03  F13                 PIC X(25).
           03  F14                 PIC 99/99/99.
           03  F15                 PIC 99/99/99.
           03  F16                 PIC 99/99/99.
           03  F17                 PIC X(1).
           03  F18                 PIC X(1).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(05) VALUE "NAME ".
           03  FILLER      PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(32) VALUE
           "110 120 130 140 150 160 170 180.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  SPEC-TABLE.
      * 1. ALTERNATE ADDRESS # 1
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 2. ALTERNATE ADDRESS # 2
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 3. ALTERNATE CITY
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      * 4. ALTERNATE STATE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 5. ALTERNATE ZIP CODE
           03  FILLER      PIC X(8) VALUE "E  A100 ".
      * 6. KYC ADDRESS # 1
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 7. KYC ADDRESS # 2
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 8. KYC CITY
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      * 9. KYC STATE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      *10. KYC ZIP CODE
           03  FILLER      PIC X(8) VALUE "E  A100 ".
      *11. KYC ID TYPE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      *12. KYC ISSUING AUTHORITY
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      *13. KYC NUMBER
           03  FILLER      PIC X(8) VALUE "E  A250 ".
      *14. KYC DATE OF ISSUE
           03  FILLER      PIC X(8) VALUE "ERNM080 ".
      *15. KYC DATE OD EXPIRATION
           03  FILLER      PIC X(8) VALUE "ERNM080 ".
      *16. KYC DATE LAST VERIFIED
           03  FILLER      PIC X(8) VALUE "ERNM080 ".
      *17. EMAIL OPT IN
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *18. TEXT OPT IN
           03  FILLER      PIC X(8) VALUE "E  A010 ".
       78  MAX                      VALUE 18.
       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/BWMAN4_DEF.CPY".
       COPY "LIBMN/BWMAN4_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  FORM-LINK-AREA   PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/BWMAN4_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ****************************
       MAIN-PROGRAM SECTION.
      ****************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BWMAN4" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO BWMAIN-LINK.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

      * NEW BORROWER TRAILER FILE, DONT WANT TO ERROR OUT (4/1/2003)
           PERFORM OPEN-BX1-FILE
           PERFORM CLOSE-BX1-FILE.

           PERFORM REC-BW-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF NEW-REC = "Y"
              MOVE "N" TO NEW-REC
              MOVE "P" TO FORM-P1
              MOVE 1 TO FORM-SEL1.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE BW-SSNO TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      ***********************************
       REC-BW-MODULE SECTION.
      ***********************************
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO BW-SSNO SOCSEC-KEY
              MOVE SPACES TO SAVEKEY
              MOVE "S  X110BWNO." TO VDU
              MOVE BW-SSNO TO VDU-SSNO-EDIT
              PERFORM SCREENIO
              GO TO READ-BWFILE.
           MOVE 0 TO NEW-FLAG.
       REC-BW-SSNO.
           MOVE "ERNX090BWNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-TYPEKEY
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-BW-SSNO.
           MOVE VDUNUM0 TO BW-SSNO HOLD-KEY.
           IF BW-SSNO = ZERO
              GO TO REC-BW-SSNO.
       READ-BWFILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO REC-BW-SSNO.
           MOVE BW-REC TO BWOR-REC.

           PERFORM DISPLAY-FIELDS.

           MOVE 0 TO NEW-FLAG.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.
       END-REC-BW-MODULE.
           EXIT.

      *    ************************************************
      *    *      ENTER SORTKEY ROUTINES
      *    *      TYPE = 1 - BORROWER SOCIAL SEC. #
      *    *             2 - BORROWER NAME
      *    *             3 - SPOUSE SOCIAL SEC. #
      *    *             4 - SPOUSE NAME
      *    ************************************************
       ENTER-TYPEKEY SECTION.
       ENTER-TYPE.
           PERFORM OPEN-BW1-FILE.
           MOVE BW-PATH-OWNBR TO BW-OWNBR.

           MOVE " 1=SS#, 2=NAME, 3=SPOUSE SS#, 4=SPOUSE NAME, F6=SCAN"
              TO MESS.
           PERFORM CALL-TMMSG.

           MOVE "EN N010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
             MOVE "CLR" TO IO-TYPE
             GO TO CLOSE-SORTKEY.
           IF VDUSTATUS = "1<"
              MOVE "SN N010TYPE." TO VDU
              MOVE 1 TO TYP VDUNUM0
              PERFORM SCREENIO
              PERFORM DISPLAY-SCAN-TYPE
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
             GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO TYP.
           IF TYP < 1 OR TYP > 4
             GO TO ENTER-TYPE.
       DISPLAY-SCAN-TYPE.
           IF TYP = 1 
              MOVE 0 TO BW-SSNO
              PERFORM START-BW1-FILE
              MOVE " ENTER SS#, OR F6 TO SCAN" TO MESS
              PERFORM CALL-TMMSG.
           IF TYP = 2 
              MOVE SPACES TO BW-NAME
              PERFORM START-BW2-FILE
              MOVE " ENTER NAME (LAST), OR F6 TO SCAN" TO MESS
              PERFORM CALL-TMMSG.
           IF TYP = 3 
              MOVE 0 TO BW-SPSSNO
              PERFORM START-BW3-FILE
              MOVE " ENTER SPOUSE SS#, OR F6 TO SCAN" TO MESS
              PERFORM CALL-TMMSG.
           IF TYP = 4 
              MOVE SPACES TO BW-SPNAME
              PERFORM START-BW4-FILE
              MOVE " ENTER SPOUSE NAME (LAST), OR F6 TO SCAN" TO MESS
              PERFORM CALL-TMMSG.
       ENTER-KEY.
           IF TYP = 2 OR TYP = 4
              MOVE "E RA240SORTKEY." TO VDU
           ELSE
              MOVE "E RA110SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SORTKEY.
           IF VDUSTATUS = "1U"
              PERFORM CLOSE-SORTKEY
              GO TO ENTER-TYPE.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE BW-REC TO BWOR-REC
                 GO TO CLOSE-SORTKEY
              ELSE
                 GO TO ENTER-KEY.
           IF TYP = 1 OR TYP = 3
              INSPECT VDUBUF REPLACING ALL " " BY "0"
              MOVE VDUBUF TO SOCSEC-KEY
              IF SOCSEC-KEY NOT NUMERIC
              GO TO ENTER-KEY.
           IF TYP = 1 OR TYP = 3
              MOVE VDUBUF TO BW-SSNO BW-SPSSNO.
           IF TYP = 2 OR TYP = 4
              PERFORM ARRANGE-BW-KEY
             MOVE BW-ALP-TABLE TO BW-NAME BW-SPNAME.
           IF TYP = 1 PERFORM START-BW1-FILE.
           IF TYP = 2 PERFORM START-BW2-FILE.
           IF TYP = 3 PERFORM START-BW3-FILE.
           IF TYP = 4 PERFORM START-BW4-FILE.
       READ-FILES-NEXT.
           IF TYP = 1 PERFORM READ-BW1-FILE-NEXT.
      *               UNLOCK BW-FILE.
           IF TYP = 2 PERFORM READ-BW2-FILE-NEXT.
           IF TYP = 3 PERFORM READ-BW3-FILE-NEXT.
           IF TYP = 4 PERFORM READ-BW4-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SORTKEY.

           MOVE "S  X110BWNO." TO VDU.
           MOVE BW-SSNO TO HOLD-KEY SOCSEC-KEY VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       CLOSE-SORTKEY.
           IF TYP = 1 PERFORM CLOSE-BW1-FILE.
           IF TYP = 2 PERFORM CLOSE-BW2-FILE.
           IF TYP = 3 PERFORM CLOSE-BW3-FILE.
           IF TYP = 4 PERFORM CLOSE-BW4-FILE.
       END-SORTKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
           MOVE "S  A010TYPE." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A160SORTKEY." TO VDU.
           PERFORM SCREENIO.

      ***********************************
       ARRANGE-BW-KEY SECTION.
      ***********************************
           MOVE 1 TO IX1 IX2.
           MOVE SPACES TO BW-ALP-TABLE.
       ARR-BW-KEY-AGAIN.
           IF IX1 NOT < 35
              GO TO ARR-BW-KEY-EXIT.
           IF VDUTAB(IX1) NOT = ","
              MOVE VDUTAB(IX1) TO BW-ALP-TBL(IX2)
            ELSE
              MOVE 35 TO IX2.
           ADD 1 TO IX1.
           ADD 1 TO IX2.
           GO TO ARR-BW-KEY-AGAIN.
       ARR-BW-KEY-EXIT.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 0 OR = 4
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF NOT (FORM-SEL1 = 1 OR 2 OR 3)
                    GO TO MODIFY-ROUTINE
                 ELSE
                    MOVE "RWR" TO IO-TYPE
                    GO TO EXIT-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0 OR CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG (ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC (ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F6.
           IF VDUSTATUS = "1<"
              IF ELE = 11
                 PERFORM CODE-SCAN
                 IF IO-FG NOT = 0
                    GO TO ENTER-ELE
                 ELSE
                    GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15
                 F-16 F-17 F-18
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO BX-ALT-ADR1.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO BX-ALT-ADR2.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO BX-ALT-CITY.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO BX-ALT-STATE.
           IF VDUBUF = " " AND BX-ALT-ADR1 = " " AND
                               BX-ALT-ADR2 = " "
              GO TO CREATE-ROUTINE.

           PERFORM OPEN-ST1-FILE.
           MOVE VDUBUF TO ST-CODE.
           PERFORM READ-ST1-FILE.
           IF IO-FG = 9
             GO TO ENTER-ELE.
           PERFORM CLOSE-ST1-FILE.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO BX-ALT-ZIP.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO BX-KYC-ADR1.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO BX-KYC-ADR2.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO BX-KYC-CITY.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUBUF TO BX-KYC-STATE.
           IF VDUBUF = " " AND BX-KYC-ADR1 = " " AND
                               BX-KYC-ADR2 = " "
              GO TO CREATE-ROUTINE.

           PERFORM OPEN-ST1-FILE.
           MOVE VDUBUF TO ST-CODE.
           PERFORM READ-ST1-FILE.
           IF IO-FG = 9
             GO TO ENTER-ELE.
           PERFORM CLOSE-ST1-FILE.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUBUF TO BX-KYC-ZIP.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUBUF TO BX-KYC-IDTYPE IDTYPE-CODE.
           IF IDTYPE-CODE NOT = "  "
              PERFORM GET-IDTYPE-DESC
              IF NOT IDTYPE-VALID
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUBUF TO BX-KYC-ISSAUTH.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUBUF TO BX-KYC-NUMBER.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDU-DATE-YYYYMMDD TO BX-KYC-ISSDATE.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDU-DATE-YYYYMMDD TO BX-KYC-EXPDATE.
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDU-DATE-YYYYMMDD TO BX-KYC-LAST-VERDATE.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUBUF TO BW-OPT-IN-EMAIL.
           IF NOT (BW-OPT-IN-EMAIL = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUBUF TO BW-OPT-IN-TEXT.
           IF NOT (BW-OPT-IN-TEXT = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.

      *****************************************
       WRITE-MODULE SECTION.
      *****************************************
           PERFORM OPEN-BW1-FILE.

           MOVE BW-REC TO BWCH-REC.
           MOVE BX-REC TO BXCH-REC.
           IF BWOR-REC = BWCH-REC
              IF BXOR-REC NOT = BXCH-REC
                 PERFORM UPDATE-BX-REC
              END-IF
              GO TO END-WRITE-MODULE.

           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF BW-REC NOT = BWOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE BWCH-REC TO BW-REC.

           PERFORM REWRITE-BW1-FILE.
           IF BXOR-REC NOT = BXCH-REC
              PERFORM UPDATE-BX-REC.

       END-WRITE-MODULE.
           PERFORM CLOSE-BW1-FILE.


      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BR-NO                      TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

           IF BXOR-ALT-ADR1 NOT = BXCH-ALT-ADR1
             MOVE "01 "                   TO LMC-FIELD
             MOVE "ALTADR1 "              TO LMC-DESC
             MOVE BXOR-ALT-ADR1           TO LMC-ORG-TEXT
             MOVE BXCH-ALT-ADR1           TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BXOR-ALT-ADR2 NOT = BXCH-ALT-ADR2
             MOVE "02 "                   TO LMC-FIELD
             MOVE "ALTADR2 "              TO LMC-DESC
             MOVE BXOR-ALT-ADR2           TO LMC-ORG-TEXT
             MOVE BXCH-ALT-ADR2           TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BXOR-ALT-CITY NOT = BXCH-ALT-CITY
             MOVE "03 "                   TO LMC-FIELD
             MOVE "ALTCITY "              TO LMC-DESC
             MOVE BXOR-ALT-CITY           TO LMC-ORG-TEXT
             MOVE BXCH-ALT-CITY           TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BXOR-ALT-STATE NOT = BXCH-ALT-STATE
             MOVE "04 "                   TO LMC-FIELD
             MOVE "ALTSTATE"              TO LMC-DESC
             MOVE BXOR-ALT-STATE          TO LMC-ORG-TEXT
             MOVE BXCH-ALT-STATE          TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BXOR-ALT-ZIP NOT = BXCH-ALT-ZIP
             MOVE "05 "                   TO LMC-FIELD
             MOVE "ALTZIP  "              TO LMC-DESC
             MOVE BXOR-ALT-ZIP            TO LMC-ORG-TEXT
             MOVE BXCH-ALT-ZIP            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BXOR-KYC-ADR1 NOT = BXCH-KYC-ADR1
             MOVE "06 "                   TO LMC-FIELD
             MOVE "KYC ADR1"              TO LMC-DESC
             MOVE BXOR-KYC-ADR1           TO LMC-ORG-TEXT
             MOVE BXCH-KYC-ADR1           TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BXOR-KYC-ADR2 NOT = BXCH-KYC-ADR2
             MOVE "07 "                   TO LMC-FIELD
             MOVE "KYC ADR2"              TO LMC-DESC
             MOVE BXOR-KYC-ADR2           TO LMC-ORG-TEXT
             MOVE BXCH-KYC-ADR2           TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BXOR-KYC-CITY NOT = BXCH-KYC-CITY
             MOVE "08 "                   TO LMC-FIELD
             MOVE "KYC CITY"              TO LMC-DESC
             MOVE BXOR-KYC-CITY           TO LMC-ORG-TEXT
             MOVE BXCH-KYC-CITY           TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BXOR-KYC-STATE NOT = BXCH-KYC-STATE
             MOVE "09 "                   TO LMC-FIELD
             MOVE "KYCSTATE"              TO LMC-DESC
             MOVE BXOR-KYC-STATE          TO LMC-ORG-TEXT
             MOVE BXCH-KYC-STATE          TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BXOR-KYC-ZIP NOT = BXCH-KYC-ZIP
             MOVE "10 "                   TO LMC-FIELD
             MOVE "KYC ZIP "              TO LMC-DESC
             MOVE BXOR-KYC-ZIP            TO LMC-ORG-TEXT
             MOVE BXCH-KYC-ZIP            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BXOR-KYC-IDTYPE NOT = BXCH-KYC-IDTYPE
             MOVE "11 "                   TO LMC-FIELD
             MOVE "ID TYPE "              TO LMC-DESC
             MOVE BXOR-KYC-IDTYPE         TO LMC-ORG-TEXT
             MOVE BXCH-KYC-IDTYPE         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BXOR-KYC-ISSAUTH NOT = BXCH-KYC-ISSAUTH
             MOVE "12 "                   TO LMC-FIELD
             MOVE "ISS AUTH"              TO LMC-DESC
             MOVE BXOR-KYC-ISSAUTH        TO LMC-ORG-TEXT
             MOVE BXCH-KYC-ISSAUTH        TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BXOR-KYC-NUMBER NOT = BXCH-KYC-NUMBER
             MOVE "13 "                   TO LMC-FIELD
             MOVE "KYC NUM "              TO LMC-DESC
             MOVE BXOR-KYC-NUMBER         TO LMC-ORG-TEXT
             MOVE BXCH-KYC-NUMBER         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BXOR-KYC-ISSDATE NOT = BXCH-KYC-ISSDATE
             MOVE "14 "                   TO LMC-FIELD
             MOVE "ISS DTE "              TO LMC-DESC
             MOVE BXOR-KYC-ISSDATE         TO LMC-ORG-DATE
             MOVE BXCH-KYC-ISSDATE         TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

           IF BXOR-KYC-EXPDATE NOT = BXCH-KYC-EXPDATE
             MOVE "15 "                   TO LMC-FIELD
             MOVE "EXP DTE "              TO LMC-DESC
             MOVE BXOR-KYC-EXPDATE         TO LMC-ORG-DATE
             MOVE BXCH-KYC-EXPDATE         TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

           IF BXOR-KYC-LAST-VERDATE NOT = BXCH-KYC-LAST-VERDATE
             MOVE "16 "                   TO LMC-FIELD
             MOVE "LSTVERDT"              TO LMC-DESC
             MOVE BXOR-KYC-LAST-VERDATE    TO LMC-ORG-DATE
             MOVE BXCH-KYC-LAST-VERDATE    TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-OPT-IN-EMAIL NOT = BWCH-OPT-IN-EMAIL
             MOVE "17 "                   TO LMC-FIELD
             MOVE "EMAILOPT"              TO LMC-DESC
             MOVE BWOR-OPT-IN-EMAIL        TO LMC-ORG-TEXT
             MOVE BWCH-OPT-IN-EMAIL        TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-OPT-IN-TEXT NOT = BWCH-OPT-IN-TEXT
             MOVE "18 "                   TO LMC-FIELD
             MOVE "TEXT OPT"              TO LMC-DESC
             MOVE BWOR-OPT-IN-TEXT         TO LMC-ORG-TEXT
             MOVE BWCH-OPT-IN-TEXT         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

       LOG-MAINTENANCE-CHGS-EXIT.
           EXIT.

      ******************************************************************
       LOG-FIELD-CHANGE SECTION.
      ******************************************************************

           MOVE "F"           TO ERRLOGW-ACTION.
           MOVE LMC-DETAIL    TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       LOG-FIELD-CHANGE-EXIT.
           EXIT.

      *****************************************
       DISPLAY-FIELDS SECTION.
      *****************************************

           MOVE SPACES TO DISPLAY-BUF.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                 INTO D-NAME.

           PERFORM OPEN-BX1-FILE.
           MOVE BW-SSNO TO BX-SSNO.
           PERFORM READ-BX1-FILE.
           PERFORM CLOSE-BX1-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-BX-FILE
              MOVE BW-SSNO TO BX-SSNO
              MOVE BW-OWNBR TO BX-BRNO.
           MOVE BX-REC TO BXOR-REC.

           MOVE BX-ALT-ADR1 TO F01.
           MOVE BX-ALT-ADR2 TO F02.
           MOVE BX-ALT-CITY TO F03.
           MOVE BX-ALT-STATE TO F04.
           MOVE BX-ALT-ZIP TO F05.
           MOVE BX-KYC-ADR1 TO F06.
           MOVE BX-KYC-ADR2 TO F07.
           MOVE BX-KYC-CITY TO F08.
           MOVE BX-KYC-STATE TO F09.
           MOVE BX-KYC-ZIP TO F10.
           MOVE BX-KYC-IDTYPE TO F11.
           MOVE BX-KYC-ISSAUTH TO F12.
           MOVE BX-KYC-NUMBER TO F13.

           IF BX-KYC-ISSDATE NOT = 0
              MOVE BX-KYC-ISSDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO F14.

           IF BX-KYC-EXPDATE NOT = 0
              MOVE BX-KYC-EXPDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO F15.

           IF BX-KYC-LAST-VERDATE NOT = 0
              MOVE BX-KYC-LAST-VERDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO F16.   

           MOVE BW-OPT-IN-EMAIL TO F17.
           MOVE BW-OPT-IN-TEXT TO F18.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      *******************************************************
       UPDATE-BX-REC SECTION.
      *******************************************************

      * BORROWER TRAILER FILE WITH EMAIL AND SCORES
           PERFORM OPEN-BX1-FILE.

           MOVE BW-SSNO TO BX-SSNO.
           PERFORM READ-BX1-FILE.

           MOVE BXCH-REC TO BX-REC.
           IF IO-FG = 0
              PERFORM REWRITE-BX1-FILE
           ELSE
              PERFORM WRITE-BX1-FILE.

       EXIT-UPDATE-BX-REC.
           PERFORM CLOSE-BX1-FILE.

      *****************************************
       CODE-SCAN SECTION.
      *****************************************
           MOVE SPACES TO CDSCAN-WINDOW-BUF.
           MOVE "ID"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A020110." TO VDU.
           MOVE CDSCAN-CODE TO VDUBUF IDTYPE-CODE.
           PERFORM SCREENIO.

           PERFORM GET-IDTYPE-DESC.
           MOVE 0 TO IO-FG.
           MOVE CDSCAN-CODE TO VDUBUF IDTYPE-CODE.
       EXIT-CODE-SCAN.
           EXIT.

      *****************************************
       GET-IDTYPE-DESC SECTION.
      *****************************************
           MOVE "** CODE NOT FOUND **" TO IDTYPE-DESC.
           MOVE "N" TO IDTYPE-VALID-YN.

           MOVE "ID"        TO CD-TYPE.
           MOVE IDTYPE-CODE TO CD-CODE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           IF IO-FG = 0
              MOVE "Y" TO IDTYPE-VALID-YN
              MOVE CD-DESC TO IDTYPE-DESC.

           PERFORM CLOSE-CD1-FILE.
       EXIT-GET-IDTYPE-DESC.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPBXCL.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO BWMAN4-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWMAN4-SYSDATE.
           DISPLAY BWMAN4-SCREEN UPON BWMAIN-WINDOW-HANDLE.
           MOVE BWMAN4-FORM-FIELDS TO WR-BUF.
           MOVE BWMAN4-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE BWMAN4-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWMAN4-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/BWMAN4_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **********************************
       MISC-PARAGRAPHS SECTION.
      **********************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      **********************************
       IO-ROUTINE SECTION.
      **********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBX1IN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
      **********************************
       END-ROUTINE SECTION.
      **********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWMAN4-SCREEN.
           MOVE BWMAIN-LINK TO FORM-LINK-AREA.
       STOP-RUN.
           EXIT PROGRAM.
