       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       BWMAN3.
       DATE-WRITTEN. 2000/05/01.
      **************************************************************************
      *  DESCRIPTION:   BORROWER MASTER FILE MAINTENANCE  PAGE 3
      *
      * REV:
      *  MJD 000502 NEW PROGRAM. COMBINES MISC PHONE NUMBERS FROM PAGE 2
      *             AND 10 CONTACT PHONE NUMBERS FROM BM-REC (TFC PR#37)
      *  MJD 000608 ADDED GP-REF-PHONE TO LOCK OUT CONTACT PHONE NUMBER ENTRY
      *  BAH 021007 IF PHONE CONTACTS ARE SPACED OUT (DELETED), THEN DELETE
      *             THE BORROWER MEMO RECORD !! TFC
      *  BAH 030402 ADDED BX-FILE (BORROWER TRAILER) AND FIELD 17. EMAIL,
      *             LENDMARK PR# 2044
      *  BAH 060802 ADDED BW-BAD-MISC-PHONE1-FG AND PHONE2-FG, REGACC PR# 142
      *  AMM 070326 CREATED A LOG FIELD CHANGE ROUTINE
      *  BAH 160513 NEW BWFILE, REMOVED BX
      *  BAH 190129 SPLIT CDFILE
      *  BLM 210924 FIX CAST ERROR IF ALPHA ENTERED IN SSNO SCAN
      *  BAH 2023.1006 CHANGED SCREEN FIELD EMAIL TO MUST Y TO ALLOW LOWERCASE
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
           "@(#)MN/BWMAN3.CBL S35 02/23/24-10:21:10 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

      *=================================================================
      *   BWCH-REC FROM 'REPLACING LEADING' ON BW-REC. [BWCH-REC]
      *   BWOR-REC FROM 'REPLACING LEADING' ON BW-REC. [BWOR-REC]
      *=================================================================
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "BWCH".
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "BWOR".

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
       01  SUB             PIC 999     VALUE 0.

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
       01  SCAN-ERR        PIC X       VALUE "Y".

       01  HOLD-KEY        PIC 9(9).
       01  SOCSEC-KEY      PIC 9(9).


       01  BW-ALP-TABLE.
           03  BW-ALP-TBL  OCCURS 70 TIMES
                           PIC X.

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
           03  PHONE-BUF           OCCURS 6.
               05  D-PHONE        PIC X(12).
               05  D-PHONE-EXT    PIC ZZZZ.
               05  D-PHONE-CD     PIC XX.
               05  D-PHONE-DESC   PIC X(14).

           03  CONTACT-BUF OCCURS 10 TIMES.
               05  D-CONTACT-DESC  PIC X(46).
               05  D-CONTACT-PHONE PIC X(12).
               05  D-CONTACT-EXT   PIC Z(04).
           03  F17                 PIC X(40).
           03  D-BAD-PHONE-1       PIC X.
           03  D-BAD-PHONE-2       PIC X.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(05) VALUE "NAME ".
           03  FILLER      PIC X(56) VALUE
           "010 011 012 013 020 021 022 023 030 031 032 033 ".
           03  FILLER      PIC X(56) VALUE
           "040 041 042 043 050 051 052 053 060 061 062 063 ".
           03  FILLER      PIC X(56) VALUE
           "070 071 072 080 081 082 090 091 092 ".
           03  FILLER      PIC X(36) VALUE
           "100 101 102 110 111 112 120 121 122 ".
           03  FILLER      PIC X(36) VALUE
           "130 131 132 140 141 142 150 151 152 ".
           03  FILLER      PIC X(24) VALUE
           "160 161 162 170 014 024.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  SPEC-TABLE.
      * 1. MISC PHONE NUMBER #1
           03  FILLER      PIC X(8) VALUE "E  A012 ".
      * 2. MISC PHONE NUMBER #2
           03  FILLER      PIC X(8) VALUE "E  A012 ".
      * 3. MISC PHONE NUMBER #3
           03  FILLER      PIC X(8) VALUE "E  A012 ".
      * 4. MISC PHONE NUMBER #4
           03  FILLER      PIC X(8) VALUE "E  A012 ".
      * 5. MISC PHONE NUMBER #5
           03  FILLER      PIC X(8) VALUE "E  A012 ".
      * 6. MISC PHONE NUMBER #6
           03  FILLER      PIC X(8) VALUE "E  A012 ".
      * 7. CONTACT DESC #1
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      * 8. CONTACT DESC #2
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      * 9. CONTACT DESC #3
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *10. CONTACT DESC #4
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *11. CONTACT DESC #5
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *12. CONTACT DESC #6
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *13. CONTACT DESC #7
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *14. CONTACT DESC #8
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *15. CONTACT DESC #9
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *16. CONTACT DESC #10
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *17. EMAIL
           03  FILLER      PIC X(8) VALUE "E  A400 ".
       78  MAX                      VALUE 17.
       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/BWMAN3_DEF.CPY".
       COPY "LIBMN/BWMAN3_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  FORM-LINK-AREA      PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/BWMAN3_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ****************************
      *      MAIN PROGRAM MODULE
      ****************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BWMAN3" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO BWMAIN-LINK.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM GET-GPENV.
           IF GP-REF-PHONE NOT = "Y"
              PERFORM PROTECT-REF-PHONE.

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
           MOVE "ERNX110BWNO." TO VDU.
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
              IF FORM-SEL1 < 0 OR = 3
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF NOT (FORM-SEL1 = 1 OR 2 OR 4)
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
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-MISC F-MISC F-MISC F-MISC F-MISC F-MISC
                 F-CONTACT F-CONTACT F-CONTACT F-CONTACT F-CONTACT
                 F-CONTACT F-CONTACT F-CONTACT F-CONTACT F-CONTACT
                 F-17
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-MISC.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO BW-MISC-PHONE(ELE).
           PERFORM SEND-PHONE.
           MOVE "S  A120  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           IF BW-MISC-PHONE(ELE) = 0
              MOVE "SNNN040  1." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO BW-MISC-EXT(ELE) VDUNUM0
              PERFORM SCREENIO
              MOVE "S  A020  2." TO VDU
              MOVE ELE TO VDUROW
              MOVE SPACES TO BW-MISC-PHONE-CD(ELE) VDUBUF
              PERFORM SCREENIO
              MOVE "S  A014  3." TO VDU
              MOVE ELE TO VDUROW
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A010  4." TO VDU
              MOVE ELE TO VDUROW
              IF ELE = 1
                 MOVE SPACES TO BW-BAD-MISC-PHONE1-FG VDUBUF
              ELSE
                 MOVE SPACES TO BW-BAD-MISC-PHONE2-FG VDUBUF
              END-IF
              GO TO CREATE-ROUTINE.
       F-EXT.
           MOVE "ENNN040  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-EXT.
           MOVE VDUNUM0 TO BW-MISC-EXT(ELE).
       F-CODE.
           MOVE "E  A020  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-EXT.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF SCAN-ERR = "Y"
                 GO TO F-CODE
              ELSE
                 MOVE "00" TO VDUSTATUS
                 MOVE CDSCAN-CODE TO VDUBUF.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-EXT.

           MOVE "PH"   TO CD-TYPE.
           MOVE VDUBUF TO BW-MISC-PHONE-CD(ELE) CD-CODE.
           PERFORM GET-CD.
           MOVE "S  A014  3." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.

           IF IO-FG NOT = 0
              GO TO F-CODE.

       F-BAD.
           IF GP-USE-BW-BAD-FLAGS = "Y"
              IF ELE = 1 OR 2
                 MOVE "E  A010  4." TO VDU
                 MOVE ELE TO VDUROW
                 PERFORM SCREENIO
                 IF VDUSTATUS = "1U"
                    GO TO F-CODE
                 ELSE
                    IF VDUBUF NOT = "Y" AND NOT = "N"
                       GO TO F-BAD
                    ELSE
                       IF ELE = 1
                          MOVE VDUBUF TO BW-BAD-MISC-PHONE1-FG
                       ELSE
                          MOVE VDUBUF TO BW-BAD-MISC-PHONE2-FG.
           GO TO CREATE-ROUTINE.

       F-CONTACT.
           COMPUTE SUB = ELE - 6.
           MOVE VDUBUF TO BW-CONTACT-DESC(SUB).
           IF BW-CONTACT-DESC(SUB) = SPACES
              MOVE 0 TO BW-CONTACT-PHONE(SUB)
              MOVE "S  A020  1." TO VDU
              MOVE ELE TO VDUROW
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "SNNN040  2." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO BW-CONTACT-PHONE(SUB) VDUNUM0
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       F-CONTACT-PHONE.
           MOVE "E  A120  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-CONTACT-PHONE.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO F-CONTACT-PHONE.
           MOVE TEL-BUF TO BW-CONTACT-PHONE(SUB).
           PERFORM SEND-PHONE.
           MOVE "S  A120  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
       F-CONTACT-EXT.
           MOVE "ENNN040  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-CONTACT-PHONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-CONTACT-EXT.
           MOVE VDUNUM0 TO BW-CONTACT-EXT(SUB).
           GO TO CREATE-ROUTINE.

       F-17.
           MOVE VDUBUF TO BW-EMAIL.
           GO TO CREATE-ROUTINE.


       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.

      *****************************
       WRITE-MODULE SECTION.
      *****************************
           PERFORM OPEN-BW1-FILE.

           MOVE BW-REC TO BWCH-REC.

           IF BWOR-REC = BWCH-REC
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
       END-WRITE-MODULE.
           PERFORM CLOSE-BW1-FILE.


      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************

           MOVE SPACES TO DISPLAY-BUF.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                INTO D-NAME.

      * BORROWER MISC PHONE #'S 1-6 (FROM BW-REC)

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF BW-MISC-PHONE(SUB) NOT = 0
                 MOVE BW-MISC-PHONE(SUB)    TO TEL-BUF
                 PERFORM SEND-PHONE
                 MOVE TEL-EDIT              TO D-PHONE(SUB)
                 MOVE BW-MISC-EXT(SUB)      TO D-PHONE-EXT(SUB)
                 MOVE "PH"                  TO CD-TYPE
                 MOVE BW-MISC-PHONE-CD(SUB) TO D-PHONE-CD(SUB) CD-CODE
                 PERFORM GET-CD
                 MOVE CD-DESC               TO D-PHONE-DESC(SUB)
              END-IF
           END-PERFORM.

      * BORROWER CONTACT INFO 1-10 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF BW-CONTACT-DESC(SUB) NOT = SPACES
                 IF BW-CONTACT-PHONE(SUB) = 0
                    MOVE SPACES TO D-CONTACT-PHONE(SUB)
                 ELSE
                    MOVE BW-CONTACT-PHONE(SUB) TO TEL-BUF
                    PERFORM SEND-PHONE
                    MOVE TEL-EDIT TO D-CONTACT-PHONE(SUB)
                 END-IF
                 MOVE BW-CONTACT-EXT(SUB) TO D-CONTACT-EXT(SUB)
                 MOVE BW-CONTACT-DESC(SUB) TO D-CONTACT-DESC(SUB)
              END-IF
           END-PERFORM.

           MOVE BW-EMAIL TO F17.
           MOVE BW-BAD-MISC-PHONE1-FG TO D-BAD-PHONE-1.
           MOVE BW-BAD-MISC-PHONE2-FG TO D-BAD-PHONE-2.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      ************************************
       CODE-SCAN SECTION.
      ************************************
           MOVE "Y" TO SCAN-ERR.
           MOVE "PH"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE "Y" TO SCAN-ERR
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A020  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "PH"        TO CD-TYPE.
           MOVE CDSCAN-CODE TO CD-CODE.
           PERFORM GET-CD.

           MOVE "S  A014  3." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "N" TO SCAN-ERR.

       EXIT-CODE-SCAN.
           EXIT.


      ************************************
       PROTECT-REF-PHONE SECTION.
      ************************************
           MOVE "N" TO CHFG(07) CHFG(08) CHFG(09) CHFG(10) CHFG(11)
                       CHFG(12) CHFG(13) CHFG(14) CHFG(15) CHFG(16).
       PROTECT-REF-PHONE-EXIT.
           EXIT.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BR-NO                      TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

           IF ( BWOR-MISC-PHONE(01) NOT = BWCH-MISC-PHONE(01) )
              MOVE "01 "                   TO LMC-FIELD
              MOVE "PHONE   "              TO LMC-DESC
              MOVE BWOR-MISC-PHONE(01)     TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE BWCH-MISC-PHONE(01)     TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-MISC-EXT(01) NOT = BWCH-MISC-EXT(01) )
              MOVE "01 "                   TO LMC-FIELD
              MOVE "PH. EXT "              TO LMC-DESC
              MOVE BWOR-MISC-EXT(01)       TO LMC-ORG-NUM
              MOVE BWCH-MISC-EXT(01)       TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-MISC-PHONE(02) NOT = BWCH-MISC-PHONE(02) )
              MOVE "02 "                   TO LMC-FIELD
              MOVE "PHONE   "              TO LMC-DESC
              MOVE BWOR-MISC-PHONE(02)     TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE BWCH-MISC-PHONE(02)     TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-MISC-EXT(02) NOT = BWCH-MISC-EXT(02) )
              MOVE "02 "                   TO LMC-FIELD
              MOVE "PH. EXT "              TO LMC-DESC
              MOVE BWOR-MISC-EXT(02)       TO LMC-ORG-NUM
              MOVE BWCH-MISC-EXT(02)       TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

       
           IF ( BWOR-MISC-PHONE(03) NOT = BWCH-MISC-PHONE(03) )
              MOVE "03 "                   TO LMC-FIELD
              MOVE "PHONE   "              TO LMC-DESC
              MOVE BWOR-MISC-PHONE(03)     TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE BWCH-MISC-PHONE(03)     TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-MISC-EXT(03) NOT = BWCH-MISC-EXT(03) )
              MOVE "03 "                   TO LMC-FIELD
              MOVE "PH. EXT "              TO LMC-DESC
              MOVE BWOR-MISC-EXT(03)       TO LMC-ORG-NUM
              MOVE BWCH-MISC-EXT(03)       TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

       
           IF ( BWOR-MISC-PHONE(04) NOT = BWCH-MISC-PHONE(04) )
              MOVE "04 "                   TO LMC-FIELD
              MOVE "PHONE   "              TO LMC-DESC
              MOVE BWOR-MISC-PHONE(04)     TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE BWCH-MISC-PHONE(04)     TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-MISC-EXT(04) NOT = BWCH-MISC-EXT(04) )
              MOVE "04 "                   TO LMC-FIELD
              MOVE "PH. EXT "              TO LMC-DESC
              MOVE BWOR-MISC-EXT(04)       TO LMC-ORG-NUM
              MOVE BWCH-MISC-EXT(04)       TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

       
           IF ( BWOR-MISC-PHONE(05) NOT = BWCH-MISC-PHONE(05) )
              MOVE "05 "                   TO LMC-FIELD
              MOVE "PHONE   "              TO LMC-DESC
              MOVE BWOR-MISC-PHONE(05)     TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE BWCH-MISC-PHONE(05)     TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-MISC-EXT(05) NOT = BWCH-MISC-EXT(05) )
              MOVE "05 "                   TO LMC-FIELD
              MOVE "PH. EXT "              TO LMC-DESC
              MOVE BWOR-MISC-EXT(05)       TO LMC-ORG-NUM
              MOVE BWCH-MISC-EXT(05)       TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

       
           IF ( BWOR-MISC-PHONE(06) NOT = BWCH-MISC-PHONE(06) )
              MOVE "06 "                   TO LMC-FIELD
              MOVE "PHONE   "              TO LMC-DESC
              MOVE BWOR-MISC-PHONE(06)     TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE BWCH-MISC-PHONE(06)     TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-MISC-EXT(06) NOT = BWCH-MISC-EXT(06) )
              MOVE "06 "                   TO LMC-FIELD
              MOVE "PH. EXT "              TO LMC-DESC
              MOVE BWOR-MISC-EXT(06)       TO LMC-ORG-NUM
              MOVE BWCH-MISC-EXT(06)       TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

       
           IF ( BWOR-BAD-MISC-PHONE1-FG NOT = BWCH-BAD-MISC-PHONE1-FG )
              MOVE "01 "                   TO LMC-FIELD
              MOVE "PHONE1FG"              TO LMC-DESC
              MOVE BWOR-BAD-MISC-PHONE1-FG TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE BWCH-BAD-MISC-PHONE1-FG TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

       
           IF ( BWOR-BAD-MISC-PHONE2-FG NOT = BWCH-BAD-MISC-PHONE2-FG )
              MOVE "02 "                   TO LMC-FIELD
              MOVE "PHONE2FG"              TO LMC-DESC
              MOVE BWOR-BAD-MISC-PHONE2-FG TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE BWCH-BAD-MISC-PHONE2-FG TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

       

           IF ( BWOR-CONTACT-PHONE(01) NOT = BWCH-CONTACT-PHONE(01) )
              MOVE "07 "                   TO LMC-FIELD
              MOVE "CONT. PH"              TO LMC-DESC
              MOVE BWOR-CONTACT-PHONE(01)  TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE BWCH-CONTACT-PHONE(01)  TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-CONTACT-PHONE(01) NOT = BWCH-CONTACT-PHONE(01) )
              MOVE "07 "                   TO LMC-FIELD
              MOVE "CONT. EX"              TO LMC-DESC
              MOVE BWOR-CONTACT-EXT(01)    TO LMC-ORG-NUM
              MOVE BWCH-CONTACT-EXT(01)    TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-CONTACT-PHONE(02) NOT = BWCH-CONTACT-PHONE(02) )
              MOVE "08 "                   TO LMC-FIELD
              MOVE "CONT. PH"              TO LMC-DESC
              MOVE BWOR-CONTACT-PHONE(02)  TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE BWCH-CONTACT-PHONE(02)  TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-CONTACT-PHONE(02) NOT = BWCH-CONTACT-PHONE(02) )
              MOVE "08 "                   TO LMC-FIELD
              MOVE "CONT. EX"              TO LMC-DESC
              MOVE BWOR-CONTACT-EXT(02)    TO LMC-ORG-NUM
              MOVE  BWCH-CONTACT-EXT(02)    TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-CONTACT-PHONE(03) NOT = BWCH-CONTACT-PHONE(03) )
              MOVE "09 "                   TO LMC-FIELD
              MOVE "CONT. PH"              TO LMC-DESC
              MOVE BWOR-CONTACT-PHONE(03)  TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE BWCH-CONTACT-PHONE(03)  TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-CONTACT-PHONE(03) NOT = BWCH-CONTACT-PHONE(03) )
              MOVE "09 "                   TO LMC-FIELD
              MOVE "CONT. EX"              TO LMC-DESC
              MOVE BWOR-CONTACT-EXT(03)    TO LMC-ORG-NUM
              MOVE BWCH-CONTACT-EXT(03)    TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

       
           IF ( BWOR-CONTACT-PHONE(04) NOT = BWCH-CONTACT-PHONE(04) )
              MOVE "10 "                   TO LMC-FIELD
              MOVE "CONT. PH"              TO LMC-DESC
              MOVE BWOR-CONTACT-PHONE(04)  TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE  BWCH-CONTACT-PHONE(04)  TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-CONTACT-PHONE(04) NOT = BWCH-CONTACT-PHONE(04) )
              MOVE "10 "                   TO LMC-FIELD
              MOVE "CONT. EX"              TO LMC-DESC
              MOVE BWOR-CONTACT-EXT(04)    TO LMC-ORG-NUM
              MOVE  BWCH-CONTACT-EXT(04)    TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

       
           IF ( BWOR-CONTACT-PHONE(05) NOT = BWCH-CONTACT-PHONE(05) )
              MOVE "11 "                   TO LMC-FIELD
              MOVE "CONT. PH"              TO LMC-DESC
              MOVE BWOR-CONTACT-PHONE(05)  TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE  BWCH-CONTACT-PHONE(05)  TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-CONTACT-PHONE(05) NOT = BWCH-CONTACT-PHONE(05) )
              MOVE "11 "                   TO LMC-FIELD
              MOVE "CONT. EX"              TO LMC-DESC
              MOVE BWOR-CONTACT-EXT(05)    TO LMC-ORG-NUM
              MOVE  BWCH-CONTACT-EXT(05)    TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

       
           IF ( BWOR-CONTACT-PHONE(06) NOT = BWCH-CONTACT-PHONE(06) )
              MOVE "12 "                   TO LMC-FIELD
              MOVE "CONT. PH"              TO LMC-DESC
              MOVE BWOR-CONTACT-PHONE(06)  TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE  BWCH-CONTACT-PHONE(06)  TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-CONTACT-PHONE(06) NOT = BWCH-CONTACT-PHONE(06) )
              MOVE "12 "                   TO LMC-FIELD
              MOVE "CONT. EX"              TO LMC-DESC
              MOVE BWOR-CONTACT-EXT(06)    TO LMC-ORG-NUM
              MOVE  BWCH-CONTACT-EXT(06)    TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

       
           IF ( BWOR-CONTACT-PHONE(07) NOT = BWCH-CONTACT-PHONE(07) )
              MOVE "13 "                   TO LMC-FIELD
              MOVE "CONT. PH"              TO LMC-DESC
              MOVE BWOR-CONTACT-PHONE(07)  TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE  BWCH-CONTACT-PHONE(07)  TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-CONTACT-PHONE(07) NOT = BWCH-CONTACT-PHONE(07) )
              MOVE "13 "                   TO LMC-FIELD
              MOVE "CONT. EX"              TO LMC-DESC
              MOVE BWOR-CONTACT-EXT(07)    TO LMC-ORG-NUM
              MOVE  BWCH-CONTACT-EXT(07)    TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

       
           IF ( BWOR-CONTACT-PHONE(08) NOT = BWCH-CONTACT-PHONE(08) )
              MOVE "14 "                   TO LMC-FIELD
              MOVE "CONT. PH"              TO LMC-DESC
              MOVE BWOR-CONTACT-PHONE(08)  TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE  BWCH-CONTACT-PHONE(08)  TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-CONTACT-PHONE(08) NOT = BWCH-CONTACT-PHONE(08) )
              MOVE "14 "                   TO LMC-FIELD
              MOVE "CONT. EX"              TO LMC-DESC
              MOVE BWOR-CONTACT-EXT(08)    TO LMC-ORG-NUM
              MOVE  BWCH-CONTACT-EXT(08)    TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

       
           IF ( BWOR-CONTACT-PHONE(09) NOT = BWCH-CONTACT-PHONE(09) )
              MOVE "15 "                   TO LMC-FIELD
              MOVE "CONT. PH"              TO LMC-DESC
              MOVE BWOR-CONTACT-PHONE(09)  TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE  BWCH-CONTACT-PHONE(09)  TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-CONTACT-PHONE(09) NOT = BWCH-CONTACT-PHONE(09) )
              MOVE "15 "                   TO LMC-FIELD
              MOVE "CONT. EX"              TO LMC-DESC
              MOVE BWOR-CONTACT-EXT(09)    TO LMC-ORG-NUM
              MOVE  BWCH-CONTACT-EXT(09)    TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

       
           IF ( BWOR-CONTACT-PHONE(10) NOT = BWCH-CONTACT-PHONE(10) )
              MOVE "16 "                   TO LMC-FIELD
              MOVE "CONT. PH"              TO LMC-DESC
              MOVE BWOR-CONTACT-PHONE(10)  TO LMC-ORG-PHONE
              INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
              MOVE  BWCH-CONTACT-PHONE(10)  TO LMC-CHG-PHONE
              INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-CONTACT-PHONE(10) NOT = BWCH-CONTACT-PHONE(10) )
              MOVE "16 "                   TO LMC-FIELD
              MOVE "CONT. EX"              TO LMC-DESC
              MOVE BWOR-CONTACT-EXT(10)    TO LMC-ORG-NUM
              MOVE  BWCH-CONTACT-EXT(10)    TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

           IF ( BWOR-EMAIL NOT = BWCH-EMAIL )
              MOVE "17 "                   TO LMC-FIELD
              MOVE "EMAIL   "              TO LMC-DESC
              MOVE BWOR-EMAIL              TO LMC-ORG-TEXT
              MOVE BWCH-EMAIL              TO LMC-CHG-TEXT
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

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO BWMAN3-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWMAN3-SYSDATE.
           DISPLAY BWMAN3-SCREEN UPON BWMAIN-WINDOW-HANDLE.
           MOVE BWMAN3-FORM-FIELDS TO WR-BUF.
           MOVE BWMAN3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE BWMAN3-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWMAN3-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/BWMAN3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************
       MISC-PARAGRAPHS SECTION.
      ************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID CODE" TO CD-DESC.
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

      *************************************
       IO-ROUTINE SECTION.
      *************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *************************************
       END-ROUTINE SECTION.
      *************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWMAN3-SCREEN.
           MOVE BWMAIN-LINK TO FORM-LINK-AREA.
       STOP-RUN.
           EXIT PROGRAM.
