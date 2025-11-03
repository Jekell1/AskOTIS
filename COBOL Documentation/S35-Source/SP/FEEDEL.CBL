       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FEEDEL.
       DATE-WRITTEN.  03/31/98.
      *********************************************************************
      *                        (SP)
      * DESCRIPTION:    DELETE FEE CODES BY BRANCH
      *
      * REV:
      * BLM 080618 IF DELETING A GIFTNET FEE CODE, DELETE THE CORRESPONDING
      *            GL TRAILER RECORD IN CTFILE, PER JIM
      * BLM 080618 CHANGED - PROGRAM WAS DOING AN ACCESS CALL ON CDFILE IN
      *            MOVE-BRANCH-INFO.  CHANGED TO JUST OPEN CD & CD IN
      *            INITIALIZATION SINCE IT'S A GLOBAL FILE
      *
      * KEC 2016.0318 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * BAH 20190729 DISPLAY CODE SELECTED FROM SCAN.
      *
      *********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/FEEDEL.CBL S34 06/19/21-12:18:45 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".

       01  PR-IFN        PIC X(2)    VALUE "PR".

      **********************
      *   MISC WORKERS     *
      **********************
       01  DISPLAY-MESS.
           03  FILLER           PIC X(25)    VALUE
           "DELETE IN PROGRESS.......".
           03  DISPLAY-MESS-BR  PIC 9(4).

       01  LN-FEED              PIC 9.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  FEECODE-BUF.
               05  FEECODE-01        PIC XX.
               05  FEECODE-02        PIC XX.
               05  FEECODE-03        PIC XX.
               05  FEECODE-04        PIC XX.
               05  FEECODE-05        PIC XX.
               05  FEECODE-06        PIC XX.
               05  FEECODE-07        PIC XX.
               05  FEECODE-08        PIC XX.
               05  FEECODE-09        PIC XX.
               05  FEECODE-10        PIC XX.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  FEECODE-LIST.
           03  FILLER          PIC X(25)  VALUE
               "FC01 FC02 FC03 FC04 FC05 ".
           03  FILLER          PIC X(25)  VALUE
               "FC06 FC07 FC08 FC09 FC10.".

       01  SV-BEG-BR           PIC 9(4)       VALUE 9999.
       01  SV-END-BR           PIC 9(4)       VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/CDBSCNW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/FEEDEL_DEF.CPY".
       COPY "LIBSP/FEEDEL_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/FEEDEL_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      *************************************
       MAIN-ROUTINE.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "FEEDEL" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE  ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE "GC"   TO CDB-TYPE.
           MOVE BR-NO TO CDB-BRANCH.
           MOVE SPACES TO CDB-CODE.

       START-CD.
           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              MOVE BR-NO TO DISPLAY-MESS-BR
              MOVE DISPLAY-MESS TO MESS
              PERFORM SEND-LEGEND.

           PERFORM START-CDB1-FILE.
       NEXT-CD.
           PERFORM READ-CDB1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-OF-GROUP.

           IF CDB-TYPE NOT = "GC"
              GO TO END-OF-GROUP.

           IF CDB-BRANCH NOT = BR-NO
              GO TO END-OF-GROUP.

           IF (FEECODE-01 NOT = SPACES AND CDB-CODE = FEECODE-01)
           OR (FEECODE-02 NOT = SPACES AND CDB-CODE = FEECODE-02)
           OR (FEECODE-03 NOT = SPACES AND CDB-CODE = FEECODE-03)
           OR (FEECODE-04 NOT = SPACES AND CDB-CODE = FEECODE-04)
           OR (FEECODE-05 NOT = SPACES AND CDB-CODE = FEECODE-05)
           OR (FEECODE-06 NOT = SPACES AND CDB-CODE = FEECODE-06)
           OR (FEECODE-07 NOT = SPACES AND CDB-CODE = FEECODE-07)
           OR (FEECODE-08 NOT = SPACES AND CDB-CODE = FEECODE-08)
           OR (FEECODE-09 NOT = SPACES AND CDB-CODE = FEECODE-09)
           OR (FEECODE-10 NOT = SPACES AND CDB-CODE = FEECODE-10)
                 MOVE "Y" TO RECORDS-FOUND
                 MOVE "N" TO FIRST-TIME
                 PERFORM DELETE-CDB1-FILE.

           GO TO NEXT-CD.

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           GO TO END-PROGRAM.

      /**********************************
      *    INITIALIZATION
      ***********************************
       INITIALIZATION SECTION.
           MOVE 0 TO Q-POS C-POS P-POS.
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF.
           MOVE "00" TO P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-CDB1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

       EO-EXEC-AUDIT SECTION.

       ENTER-PARAMETERS SECTION.
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO FC-01.

       BR-01.
           MOVE "E  A040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 999 TO END-BRANCH
              GO TO FC-01.
      *----------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE " INVALID BRANCH RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE "S  A160DISPLAY-USER." TO VDU
                 MOVE "USER BRANCH ONLY" TO VDUBUF
                 PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO FC-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       FC-01.
           MOVE "ENNA020FC01." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO FC-01
              ELSE
                 MOVE "S  A020FC01." TO VDU
                 MOVE CDBSCN-CODE    TO VDUBUF
                 PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FC-01.
           MOVE VDUBUF TO FEECODE-01.
           IF FEECODE-01 = SPACES
              MOVE SPACES TO FEECODE-BUF WR-BUF
              MOVE FEECODE-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO EXIT-PARM.

       FC-02.
           MOVE "ENNA020FC02." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FC-01.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO FC-02
              ELSE
                 MOVE "S  A020FC02." TO VDU
                 MOVE CDBSCN-CODE    TO VDUBUF
                 PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FC-02.
           MOVE VDUBUF TO FEECODE-02.
           IF FEECODE-02 = SPACES
              MOVE SPACES TO FEECODE-03 FEECODE-04
                             FEECODE-05 FEECODE-06
                             FEECODE-07 FEECODE-08
                             FEECODE-09 FEECODE-10
              MOVE FEECODE-BUF TO WR-BUF
              MOVE FEECODE-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO EXIT-PARM.

       FC-03.
           MOVE "ENNA020FC03." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FC-02.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO FC-03
              ELSE
                 MOVE "S  A020FC03." TO VDU
                 MOVE CDBSCN-CODE    TO VDUBUF
                 PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FC-03.
           MOVE VDUBUF TO FEECODE-03.
           IF FEECODE-03 = SPACES
              MOVE SPACES TO FEECODE-03 FEECODE-04
                             FEECODE-05 FEECODE-06
                             FEECODE-07 FEECODE-08
                             FEECODE-09 FEECODE-10
              MOVE FEECODE-BUF TO WR-BUF
              MOVE FEECODE-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO EXIT-PARM.

       FC-04.
           MOVE "ENNA020FC04." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FC-03.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO FC-04
              ELSE
                 MOVE "S  A020FC04." TO VDU
                 MOVE CDBSCN-CODE    TO VDUBUF
                 PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FC-04.
           MOVE VDUBUF TO FEECODE-04.
           IF FEECODE-04 = SPACES
              MOVE SPACES TO FEECODE-04 FEECODE-05
                             FEECODE-06 FEECODE-07
                             FEECODE-08 FEECODE-09
                             FEECODE-10
              MOVE FEECODE-BUF TO WR-BUF
              MOVE FEECODE-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO EXIT-PARM.

       FC-05.
           MOVE "ENNA020FC05." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FC-04.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO FC-05
              ELSE
                 MOVE "S  A020FC05." TO VDU
                 MOVE CDBSCN-CODE    TO VDUBUF
                 PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FC-05.
           MOVE VDUBUF TO FEECODE-05.
           IF FEECODE-05 = SPACES
              MOVE SPACES TO FEECODE-05 FEECODE-06
                             FEECODE-07 FEECODE-08
                             FEECODE-09 FEECODE-10
              MOVE FEECODE-BUF TO WR-BUF
              MOVE FEECODE-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO EXIT-PARM.

       FC-06.
           MOVE "ENNA020FC06." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FC-05.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO FC-06
              ELSE
                 MOVE "S  A020FC06." TO VDU
                 MOVE CDBSCN-CODE    TO VDUBUF
                 PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FC-06.
           MOVE VDUBUF TO FEECODE-06.
           IF FEECODE-06 = SPACES
              MOVE SPACES TO FEECODE-06 FEECODE-07
                             FEECODE-08 FEECODE-09
                             FEECODE-10
              MOVE FEECODE-BUF TO WR-BUF
              MOVE FEECODE-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO EXIT-PARM.

       FC-07.
           MOVE "ENNA020FC07." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FC-06.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO FC-07
              ELSE
                 MOVE "S  A020FC07." TO VDU
                 MOVE CDBSCN-CODE    TO VDUBUF
                 PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FC-07.
           MOVE VDUBUF TO FEECODE-07.
           IF FEECODE-07 = SPACES
              MOVE SPACES TO FEECODE-07 FEECODE-08
                             FEECODE-09 FEECODE-10
              MOVE FEECODE-BUF TO WR-BUF
              MOVE FEECODE-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO EXIT-PARM.

       FC-08.
           MOVE "ENNA020FC08." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FC-07.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO FC-08
              ELSE
                 MOVE "S  A020FC08." TO VDU
                 MOVE CDBSCN-CODE    TO VDUBUF
                 PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FC-08.
           MOVE VDUBUF TO FEECODE-08.
           IF FEECODE-08 = SPACES
              MOVE SPACES TO FEECODE-08 FEECODE-09 FEECODE-10
              MOVE FEECODE-BUF TO WR-BUF
              MOVE FEECODE-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO EXIT-PARM.

       FC-09.
           MOVE "ENNA020FC09." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FC-08.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO FC-09
              ELSE
                 MOVE "S  A020FC09." TO VDU
                 MOVE CDBSCN-CODE    TO VDUBUF
                 PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FC-09.
           MOVE VDUBUF TO FEECODE-09.
           IF FEECODE-09 = SPACES
              MOVE SPACES TO FEECODE-09 FEECODE-10
              MOVE FEECODE-BUF TO WR-BUF
              MOVE FEECODE-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO EXIT-PARM.

       FC-10.
           MOVE "ENNA020FC10." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FC-09.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO FC-10
              ELSE
                 MOVE "S  A020FC10." TO VDU
                 MOVE CDBSCN-CODE    TO VDUBUF
                 PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FC-10.
           MOVE VDUBUF TO FEECODE-10.

       EXIT-PARM.
           EXIT.

       DISPLAY-PARAMETERS SECTION.
       DISPLAY-PARMS.

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "SN A020FC01" TO VDU.
           MOVE FEECODE-01 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN A020FC02" TO VDU.
           MOVE FEECODE-02 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN A020FC03" TO VDU.
           MOVE FEECODE-03 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN A020FC04" TO VDU.
           MOVE FEECODE-04 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN A020FC05" TO VDU.
           MOVE FEECODE-05 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN A020FC06" TO VDU.
           MOVE FEECODE-06 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN A020FC07" TO VDU.
           MOVE FEECODE-07 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN A020FC08" TO VDU.
           MOVE FEECODE-08 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN A020FC09" TO VDU.
           MOVE FEECODE-09 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN A020FC10" TO VDU.
           MOVE FEECODE-10 TO VDUBUF.
           PERFORM SCREENIO.


      ***************************************************************
      *    C O D E   S C A N   R O U T I N E
      ***************************************************************
       CODE-SCAN SECTION.
           MOVE "GC"                TO CDBSCN-TYPE.
           MOVE EXT-CONAME-USER-LOC TO CDBSCN-BRANCH.
           MOVE SPACES              TO CDBSCN-CODE.

           MOVE "WI/CDBSCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDBSCN-STATUS NOT = "00") OR (CDBSCN-CODE = SPACES) 
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           MOVE CDBSCN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.
       EXIT-CODE-SCAN.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO FEEDEL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FEEDEL-SYSDATE.
           DISPLAY FEEDEL-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE FEEDEL-FORM-FIELDS TO WR-BUF.
           MOVE FEEDEL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE FEEDEL-HELP--FILE TO HELP-LINK-FILE.
           MOVE FEEDEL-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/FEEDEL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************
       MISC-PARAGRAPHS SECTION.
      ************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      /***************************************
      *         I / O      R O U T I N E S
      ****************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCDB1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.


           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY FEEDEL-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
