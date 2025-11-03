       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DLMAN2.
      ***************************************************************
      *                 (MN)
      *  DESCRIPTION:   DEALER MASTER FILE MAINTENANCE
      *
      *                 PAGE 2
      *
      * BAH 061397 ADDED RVPRIN FRMLA 01 FOR FEDERATED
      * BAH 040198 ADDED DL-RVPINR-FRMLA 02 FOR FUTURE PR#677
      * BAH 120514 REMOVED SPECIAL-FG
      * KEC 2016.1221 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FIELD WORKERS TO CONTAIN FILE PREFIXES (DL).
      * BAH 181011 GLOBAL DLFILE
      ***************************************************************
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
           "@(#)MN/DLMAN2.CBL S35 02/21/24-12:32:08 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".

       01  SUB             PIC 99     VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION          VALUE SPACES.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                           PIC X(2).
       01  FORM-NUM1 REDEFINES ACTION.
           03  FORM-KEY1   PIC X(4).
           03  FORM-P1     PIC X.
           03  FORM-SEL1   PIC 9.

       01  CHAN            PIC X(4).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9       VALUE 0.

      * THIS IS NEEDED FOR CLEAR-DL-FILE
       01  HOLD-DL1-KEY.
           03  HOLD-DL-BRNO            PIC 9(4).
           03  HOLD-DL-NUM             PIC 9(6).
           03  HOLD-DL-DLNO            REDEFINES HOLD-DL-NUM
                                       PIC 9(4)V99.

       01  ORIGINAL-DL-REC    PIC X(DL-SIZE).
       01  CHANGED-DL-REC     PIC X(DL-SIZE).


       01  DLMAIN-LINK        VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(6).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  DLMAIN-WINDOW-HANDLE PIC X(10).

      ***********************************************
      *    SCREEN DISPLAY BUFFER
      ***********************************************
       01  DISPLAY-BUF                  VALUE SPACES.
           03  DLNAME                   PIC X(30).
           03  A01                      PIC ZZ9.
           03  A02                      PIC ZZ9.
           03  A03                      PIC 99.
           03  A031                     PIC ZZ9.

           03  FILLER                   OCCURS 3.
               05  ARVPRIN-FRMLA        PIC 99.
               05  ARVPRIN-CODE         PIC X.
               05  ARVPRIN-ADDON        PIC ZZZZZ.ZZ-.
               05  FILLER               OCCURS 6.
                   07  ARVPRIN-TERM     PIC ZZZ.
                   07  ARVPRIN-RATE     PIC ZZ.ZZZZ.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(47) VALUE
           "DLNAME 010 020 030 031 040 050 060 070 071 080 ".
           03  FILLER      PIC X(48) VALUE
           "081 090 091 100 101 110 111 120 121 130 140 150 ".
           03  FILLER      PIC X(48) VALUE
           "160 161 170 171 180 181 190 191 200 201 210 211 ".
           03  FILLER      PIC X(48) VALUE
           "220 230 240 250 251 260 261 270 271 280 281 290 ".
           03  FILLER      PIC X(12) VALUE
           "291 300 301.".

      ***********************************************
      *    ELEMENT ENTRY LEVEL BASE
      ***********************************************
       01  LEV                  PIC 9     VALUE 0.
       01  LEV-RVPRIN-BASE-TABLE.
           03  LEV-RVPRIN-1     PIC 99 VALUE 4.
           03  LEV-RVPRIN-2     PIC 99 VALUE 13.
           03  LEV-RVPRIN-3     PIC 99 VALUE 22.
       01  LEV-RVPRIN-BASE-TBL REDEFINES LEV-RVPRIN-BASE-TABLE.
           03  LEV-RVPRIN-BASE  OCCURS 3
                                PIC 99.

       01  MAX                  PIC 99   VALUE 30.

      ***********************************************
      *    ELEMENT SPECIFICATIONS
      ***********************************************
       01  SPEC-TABLE.
      *01. MAX PURCHASE TERM
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
      *02. MAX PURCHASE PERCENT
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
      *03. EARNINGS CODE
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *04-12: RESERVE FROM PRINCIPAL #1 PARAMETERS
           03  FILLER        PIC X(8) VALUE "ENNN020 ".
           03  FILLER        PIC X(8) VALUE "E  A010 ".
           03  FILLER        PIC X(8) VALUE "ENNN052 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
      *13-21: RESERVE FROM PRINCIPAL #2 PARAMETERS
           03  FILLER        PIC X(8) VALUE "ENNN020 ".
           03  FILLER        PIC X(8) VALUE "E  A010 ".
           03  FILLER        PIC X(8) VALUE "ENNN052 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
      *22-30: RESERVE FROM PRINCIPAL #3 PARAMETERS
           03  FILLER        PIC X(8) VALUE "ENNN020 ".
           03  FILLER        PIC X(8) VALUE "E  A010 ".
           03  FILLER        PIC X(8) VALUE "ENNN052 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".
           03  FILLER        PIC X(8) VALUE "ENNN030 ".

       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 30 TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/DLMAN2_DEF.CPY".
       COPY "LIBMN/DLMAN2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/DLMAN2_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      **********************************
       MAIN-PROGRAM SECTION.
      **********************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DLMAN2" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO DLMAIN-LINK.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-DL-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF NEW-REC = "Y"
              MOVE "Y" TO NEW-REC
              MOVE "P" TO FORM-P1
              MOVE 3 TO FORM-SEL1.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE DL-NUM TO SAVEKEY                 
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      *******************************
       REC-DL-MODULE SECTION.
      *******************************
       REC-DL-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO DL-NUM                    
              MOVE SPACES TO SAVEKEY
              MOVE "SNNN042DLNO." TO VDU
              MOVE DL-DLNO TO VDUNUM2
              PERFORM SCREENIO
              GO TO READ-DLFILE.
           MOVE 0 TO NEW-FLAG.
       REC-DL-NUMBER.
           MOVE "DLNAME." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "ENNN042DLNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-DL-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-DL-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-DL-NUMBER.
           MOVE VDUNUM2 TO DL-DLNO.
           IF DL-DLNO = 0 OR DL-NO = 0
              GO TO REC-DL-NUMBER.
           MOVE DL-BRNO TO HOLD-DL-BRNO.               
           MOVE DL-NUM  TO HOLD-DL-NUM.                
       READ-DLFILE.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG NOT = 0
              GO TO REC-DL-MODULE.
           MOVE DL-REC TO ORIGINAL-DL-REC.
           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.
       END-REC-DL-MODULE.
           EXIT.

      ***********************************
       ENTER-SCANKEY SECTION.
      ***********************************
       ENTER-SCAN.
           MOVE " ENTER DEALER ABRV-NM, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-DL2-FILE.
           MOVE DL-PATH-OWNBR TO DL-BRNO.                
           MOVE SPACES        TO DL-SHORT.               


           PERFORM START-DL2-FILE.
       ENTER-KEY.
           MOVE "E RA160SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE DL-BRNO TO HOLD-DL-BRNO                 
                 MOVE DL-NUM  TO HOLD-DL-NUM                  
                 MOVE DL-REC TO ORIGINAL-DL-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE DL-PATH-OWNBR TO DL-BRNO.                      
           MOVE VDUBUF        TO DL-SHORT.                     


           PERFORM START-DL2-FILE.
       READ-FILES-NEXT.
           PERFORM READ-DL2-FILE-NEXT.
           IF IO-FG = 9 OR (DL-BRNO NOT = DL-PATH-OWNBR)         
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
      *    UNLOCK DL-FILE.
           MOVE "SNNN042DLNO." TO VDU.
           MOVE DL-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-DL2-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

      ************************************************
      *   ENTRY-MODULE                               *
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD      *
      *              1 - INITIAL NEW RECORD ENTRY    *
      *              2 - MODIFY NEW RECORD           *
      ************************************************
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
              IF FORM-SEL1 < 0 OR = 2
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF FORM-SEL1 NOT = 1 AND NOT = 3 AND NOT = 4
                                      AND NOT = 5 AND NOT = 6
                    GO TO MODIFY-ROUTINE
                 ELSE
                    MOVE "RWR" TO IO-TYPE
                    GO TO EXIT-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
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
              GO TO GOTO-FIELD1.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD1.
           IF ELE < 4
              GO TO A-01 A-02 A-03
                    DEPENDING ON ELE.
      * ENTRY OF PVPRIN RESERVE FIELDS:
           IF ELE NOT < LEV-RVPRIN-BASE(3)
              MOVE 3 TO LEV
           ELSE
              IF ELE NOT < LEV-RVPRIN-BASE(2)
                 MOVE 2 TO LEV
              ELSE
                 IF ELE NOT < LEV-RVPRIN-BASE(1)
                    MOVE 1 TO LEV.
           COMPUTE SUB = ELE - LEV-RVPRIN-BASE(LEV) + 1.
           IF SUB > 4
              MOVE 4 TO SUB.
           GO TO A-RVPRIN-FRMLA
                 A-RVPRIN-CODE
                 A-RVPRIN-ADDON
                 A-RVPRIN-TERM-RATE
                 DEPENDING ON SUB.
           GO TO END-ENTRY-MODULE.
      *********************************
      *     SET RECORD ELEMENTS       *
      *********************************
       A-01.
           MOVE VDUNUM0 TO DL-MAXPURTRM.
           IF DL-MAXPURTRM = 0
              GO TO ENTER-ELE.
           IF DL-MAXPURTRM NOT =  999
              MOVE "WARNING! THESE ARE RESTRICTED CODES," TO MESS
              PERFORM SEND-MESS
              MOVE "THIS FIELD MUST BE 999." TO MESS
              PERFORM SEND-MESS.
      *------>MOVE "UNLESS YOUR COMPANY IS ..BIG APPLE CREDIT!" TO MESS
      *------>PERFORM SEND-MESS.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUNUM0 TO DL-MAXPURPER.
           IF DL-MAXPURPER > 100 OR = 0
              GO TO ENTER-ELE.
           IF DL-MAXPURPER NOT =  100
              MOVE "WARNING! THESE ARE RESTRICTED CODES," TO MESS
              PERFORM SEND-MESS
              MOVE "THIS FIELD MUST BE 100." TO MESS
              PERFORM SEND-MESS.
      *------>MOVE "UNLESS YOUR COMPANY IS ..BIG APPLE CREDIT!" TO MESS
      *------>PERFORM SEND-MESS.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUNUM0 TO DL-EARNCD.
           IF NOT DL-EARNCD-VALID
              MOVE "ENTER 0 OR 1 ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF DL-EARNCD = 0
              MOVE "S  A030031." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE 0 TO DL-EARNFAC
              GO TO CREATE-ROUTINE.

           IF DL-EARNCD NOT = 0
              MOVE "WARNING! THESE ARE RESTRICTED CODES," TO MESS
              PERFORM SEND-MESS
              MOVE "THIS FIELD MUST BE 0." TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
      *------>MOVE "UNLESS YOUR COMPANY IS ..BIG APPLE CREDIT!" TO MESS
      *------>PERFORM SEND-MESS.
       A-031.
           MOVE "ENNN030031." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-031.
           MOVE VDUNUM0 TO DL-EARNFAC.
           IF DL-EARNFAC > 100
              GO TO A-031.
           GO TO CREATE-ROUTINE.

       A-RVPRIN-FRMLA.
           MOVE VDUNUM0 TO DL-RVPRIN-FRMLA(LEV).
           IF NOT DL-RVPRIN-FRMLA-VALID(LEV)
              MOVE "ENTER 0, 1, 2 OR 99 ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-RVPRIN-CODE.
           MOVE VDUBUF TO DL-RVPRIN-CODE(LEV).
           IF NOT DL-RVPRIN-CODE-VALID(LEV)
              MOVE "ENTER SPACE ONLY " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-RVPRIN-ADDON.
           MOVE VDUNUM2 TO DL-RVPRIN-ADDON(LEV).
           GO TO CREATE-ROUTINE.
       A-RVPRIN-TERM-RATE.
           COMPUTE SUB = ELE - (LEV-RVPRIN-BASE(LEV) + 2).
           MOVE VDUNUM0 TO DL-RVPRIN-TERM(LEV SUB).

           MOVE "EN N024" TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 1 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-RVPRIN-TERM-RATE.
           MOVE VDUNUM4 TO DL-RVPRIN-RATE(LEV SUB).
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.

      *****************************************
       WRITE-MODULE SECTION.
      *****************************************
           PERFORM OPEN-DL1-FILE.
           MOVE DL-REC TO CHANGED-DL-REC.
           IF ORIGINAL-DL-REC = CHANGED-DL-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-DL1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF DL-REC NOT = ORIGINAL-DL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-DL-REC TO DL-REC.
           PERFORM REWRITE-DL1-FILE.
       END-WRITE-MODULE.
           PERFORM CLOSE-DL1-FILE.

      *****************************************
       DISPLAY-FIELDS SECTION.
      *****************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE DL-NAME TO DLNAME.
           MOVE DL-MAXPURTRM TO A01.
           MOVE DL-MAXPURPER TO A02.
           MOVE DL-EARNCD TO A03.
           MOVE DL-EARNFAC TO A031.
           MOVE 1 TO LEV.
           PERFORM MOVE-RVPRIN.
           MOVE 2 TO LEV.
           PERFORM MOVE-RVPRIN.
           MOVE 3 TO LEV.
           PERFORM MOVE-RVPRIN.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************
       MOVE-RVPRIN SECTION.
      ******************************
           MOVE DL-RVPRIN-FRMLA(LEV) TO ARVPRIN-FRMLA(LEV).
           MOVE DL-RVPRIN-CODE(LEV) TO ARVPRIN-CODE(LEV).
           MOVE DL-RVPRIN-ADDON(LEV) TO ARVPRIN-ADDON(LEV).
           PERFORM MOVE-RVPRIN-TERM-RATE
                VARYING SUB FROM 1 BY 1 UNTIL SUB > 6.

      ******************************
       MOVE-RVPRIN-TERM-RATE SECTION.
      ******************************
           MOVE DL-RVPRIN-TERM(LEV SUB) TO ARVPRIN-TERM(LEV SUB).
           MOVE DL-RVPRIN-RATE(LEV SUB) TO ARVPRIN-RATE(LEV SUB).

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO DLMAN2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DLMAN2-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY DLMAN2-SCREEN UPON DLMAIN-WINDOW-HANDLE.
           MOVE DLMAN2-FORM-FIELDS TO WR-BUF. 
           MOVE DLMAN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 


      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE DLMAN2-HELP--FILE TO HELP-LINK-FILE.
           MOVE DLMAN2-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/DLMAN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************
       MISC-PARAGRAPHS SECTION.
      *******************************
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
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *******************************
       IO-ROUTINES SECTION.
      *******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-DL2-FILE.
           PERFORM CLOSE-DL1-FILE.
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPDL1IN.CPY".
       COPY "LIBLP/LPDL2RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
      *******************************
       END-ROUTINE SECTION.
      *******************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DLMAN2-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE DLMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
