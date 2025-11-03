       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FDPURG.
       DATE-WRITTEN.    060184.
      *****************************************************
      *   DESC: FORMS REQUEST PURGE
      *            IN END OF DAY BATCH (3/13/2023)
      *         CURRENT BRANCH ONLY - CAN NOT ENTER A DIFFERENT BRANCH!
      *      FMPURG WILL ALLOW A BRANCH RANGE, THIS ONE DOES NOT, CURRENT ONLY
      *         LOOPS THRU FMFILE, DELETES RECORDS THAT MATCH SCREEN CRITERIA
      *
      *  REV:
      * 082595 BLL FM-TEST1-KEY HAD FILLER OF 2 - NEEDED
      *            TO BE 4 TO MATCH BRNO OF 4
      * 081017 BAH ADDED PASSWORD, MODERN PR# 3267
      * BAH 181114 GLOBAL FMFILE
      * BAH 2023.0531 REMOVED THE PRINTER, NEVER PRINTED ANYTHING!
      * RMZ 2023.1115 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *****************************************************
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
           "@(#)LP/FDPURG.CBL S35 02/28/24-14:35:55 >".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS-FORM-NO          PIC 9(3).      
       01  QWS-PURGE-TYPE       PIC X.      
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  FM1-CONN-OPEN-SW     PIC X        VALUE "N".

      * FM-TEST1-KEY IS USED TO TEST FOR THE EDIT-FM-TYPE
       01  FM-TEST1-KEY.
           03  FILLER           PIC X(4).
           03  FM-TEST-TYPE     PIC 9(2).
           03  FILLER           PIC X(14).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED          PIC 99       VALUE 0.


      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9(4).
           03  END-BR          PIC 9(4).
           03  FORM-NO         PIC 999.
           03  PURGE-TYPE      PIC X.

       01  VERSION-CONTROL     PIC X VALUE "A".

       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  PURG-SEQ            VALUE 1.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/FDPURG_DEF.CPY".
       COPY "LIBLP/FDPURG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/FDPURG_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      **********************************
       MAIN-MODULE SECTION.
      **********************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "FDPURG" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE FM-PATH-OWNBR TO FM-BRNO.
           MOVE SPACES        TO FM-INITKEY.
           MOVE 0             TO FM-FMNO.

      D    STOP MESS.
           PERFORM START-FM1-FILE. *> SSP - FMFILE

       NEXT-FM.
           PERFORM READ-FM1-FILE-NEXT. *> EMBEDDED - FMFILE
           IF IO-FG = 9 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO END-ROUTINE.

      *    UNLOCK FM-FILE.

           IF FM-BRNO < BEG-BR OR > END-BR
              GO TO NEXT-FM.

           IF FORM-NO NOT = 0
              IF FM-FMNO NOT = FORM-NO
                 GO TO NEXT-FM.

           IF PURGE-TYPE = "P"
              IF FM-PRINT NOT = "Y"
                 GO TO NEXT-FM.
           IF PURGE-TYPE = "N"
              IF FM-PRINT NOT = " "
                 GO TO NEXT-FM.

      *    IF FORM-NO NOT = 0
      *       GO TO FORM-PURGE-CONT.

      *    MOVE FM-FMNO TO FD-FMNO.
      *    PERFORM READ-FD1-FILE.

      *    IF IO-FG   = 9  
           IF FD-PROG = "&&" AND
              FORM-NO = 0
              GO TO NEXT-FM
           ELSE
      * FORM-PURGE-CONT.
               MOVE FM-INITKEY TO FM-TEST1-KEY 
               IF FM-TEST-TYPE NOT NUMERIC
      *        MOVE FM-FMNO TO FD-FMNO
      *        PERFORM READ-FD1-FILE
      *        IF IO-FG = 0
                  IF FD-PROG NOT = "&&"
                     IF FD-PROG = GB-CHECK-PROG
                       IF FM-PRINT NOT = "Y"
                          GO TO NEXT-FM.

      * I DO NOT KNOW WHY THIS KEY READ IS HERE, CANT THE DATABASE SIMPLY
      * DELETE FROM THE READ-FM1-FILE-NEXT RECORD IT RETRIEVED? THE KEY
      * WOULD BE FILLED IN.

      *    PERFORM READ-FM1-FILE.
      *    IF IO-FG = 9
      *       GO TO IO-ERROR.

           PERFORM DELETE-FM1-FILE. *> EMBEDDED - FMFILE

           GO TO NEXT-FM.
       END-ROUTINE.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE 0    TO Q-POS.
           MOVE 0    TO C-POS.
           MOVE 0    TO P-POS.
           MOVE "V"  TO Q-BUF.
           MOVE "1"  TO C-BUF.
           MOVE "00" TO P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM SQL-OPEN-UPDATE-CONNECTION.

           PERFORM OPEN-FD1-FILE.
           PERFORM OPEN-FM1-FILE.

           IF FORM-NO = 0
              GO TO START-PURGE.

           MOVE FORM-NO TO FD-FMNO.
           PERFORM READ-FD1-FILE.

       START-PURGE.

           PERFORM OPEN-GB-FILE.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************
           PERFORM OPEN-FD1-FILE.
           PERFORM OPEN-FM1-FILE.

           IF FORM-NO = 0
              GO TO EXIT-AUDIT.

           MOVE FORM-NO TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG = 9
              MOVE "FORM# NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              MOVE "E" TO EXT-EOBUF-ERRCD.

       EXIT-AUDIT.
           EXIT.

      ***************************
       ENTER-PARAMETERS SECTION.
      ***************************
       ENTER-PARMS.

           IF VDUSTATUS = "1U"
              GO TO PRGTYP.

           IF EXT-ROUTE-BUF = "00"
              MOVE PURG-SEQ TO PASSWDW-SEQ
              MOVE "TO RUN FORM PURGE" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "E" TO EXT-EOBUF-ERRCD
                 MOVE "1>" TO VDUSTATUS
                 GO TO EXIT-PARM.
       BEGBRN.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "16" GO TO ALLBRN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGBRN.
           MOVE VDUNUM0 TO BEG-BR.
       ENDBRN.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BEGBRN.
           IF VDUSTATUS = "16" GO TO ALLBRN.
           IF VDUSTATUS = "1U" GO TO BEGBRN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDBRN.
           MOVE VDUNUM0 TO END-BR.
           GO TO RRFMNO.
       ALLBRN.
           MOVE "SNNN040BR1." TO VDU.
           MOVE "0000" TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE "9999" TO END-BR VDUNUM0.
           PERFORM SCREENIO.
       RRFMNO.
           MOVE "ENNN030FM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BEGBRN.
           IF VDUSTATUS = "16" GO TO ALL-FM.
           IF VDUSTATUS = "1U" GO TO BEGBRN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RRFMNO.
           MOVE VDUNUM0 TO FORM-NO.
           IF FORM-NO = "000" GO TO RRFMNO.
           GO TO PRGTYP.
       ALL-FM.
           MOVE "SNNN030FM." TO VDU.
           MOVE "000" TO FORM-NO VDUNUM0.
           PERFORM SCREENIO.
       PRGTYP.
           MOVE "E  A010PA." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BEGBRN.
           IF VDUSTATUS = "1U" GO TO RRFMNO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PRGTYP.
           MOVE VDUBUF TO PURGE-TYPE.
           IF PURGE-TYPE = "P" GO TO OK-YN.
           IF PURGE-TYPE = "N" GO TO OK-YN.
           IF PURGE-TYPE NOT = "A" GO TO PRGTYP.
       OK-YN.
       EXIT-PARM.
           EXIT.

      *****************************
       DISPLAY-PARAMETERS SECTION.
      *****************************

           MOVE "SNNN040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE END-BR TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN030FM." TO VDU.
           MOVE FORM-NO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A010PA." TO VDU.
           MOVE PURGE-TYPE TO VDUBUF.
           PERFORM SCREENIO.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************
       VERIFY-PASSWORD SECTION.
      *******************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "FDPURG" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO FDPURG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FDPURG-SYSDATE.
           DISPLAY FDPURG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE FDPURG-FORM-FIELDS TO WR-BUF.
           MOVE FDPURG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.


      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE FDPURG-HELP--FILE TO HELP-LINK-FILE.
           MOVE FDPURG-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/FDPURG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************
       MISC-PARAGRAPHS SECTION.
      *******************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".

      ******************************
       IO-ROUTINE SECTION.
      ******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLRP_SQL.CPY".
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FM1-CONNECTION.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".


      * COPY "LIBLP/LPFM1IN.CPY".
      * COPYMEMBER: LIBLP/LPFM1IN
       LOAD-FM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( FM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( FM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO FM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO FM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO FM-ALL-DATA
              MOVE FM-ALL-PATH             TO FM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( FM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO FM-ALL-BASE
              MOVE FILPATH-MACHINE         TO FM-ALL-MACHINE
              MOVE FILPATH-DATA            TO FM-ALL-DATA
              MOVE FM-ALL-PATH             TO FM-PATH.

      *-----------------------------------------------------------------
       OPEN-FM1-FILE.
           PERFORM LOAD-FM1-FILE.
           PERFORM OPEN-IT.
           MOVE FM-PATH-SQL TO E-FILE.

           IF FM1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO FM1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS FM1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-FM1-FILE. *> SSP -FMFILE
           PERFORM START-IT.
           MOVE FM-PATH-OWNBR  TO FM-BRNO.
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE FM1-KEY        TO E-KEYX.

           IF ( FM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-FM1-FILE.

           IF ( QFM1-WEND-FMNO = ZEROES    ) OR
              ( QFM1-WEND-FMNO NOT NUMERIC )
              MOVE FM-PATH-OWNBR        TO QFM1-WBEG-BRNO
              MOVE FM-PATH-OWNBR        TO QFM1-WEND-BRNO
              MOVE FM-INITKEY           TO QFM1-WBEG-INITKEY
              MOVE ALL "Z"              TO QFM1-WEND-INITKEY
              MOVE FM-FMNO              TO QFM1-WBEG-FMNO
              MOVE ALL "9"              TO QFM1-WEND-FMNO.

       
      *  STORED PROCEDURE:
      *
      *   SELECT FMFILE.FM_BRNO,
      *          FMFILE.FM_INITKEY,
      *          FMFILE.FM_FMNO,
      *          FMFILE.FM_PRINT, 
      *          ISNULL(FDFILE.FD_FMNO, 0),
      *          ISNULL(FDFILE.FD_PROG,'&&')
      * 
      *     FROM DBO.FMFILE FMFILE
      * 
      *     LEFT OUTER JOIN
      *          DBO.FDFILE FDFILE 
      *       ON  ( FMFILE.FM_FMNO              = FDFILE.FD_FMNO )
      * 
      *    WHERE  ( FMFILE.FM_BRNO        BETWEEN @QFM1_WBEG_BRNO
      *                                       AND @QFM1_WEND_BRNO )    
      *      AND (( @QWS_FORM_NO                = 0 )   
      *       OR  ( @QWS_FORM_NO               != 0
      *      AND    FMFILE.FM_FMNO              = @QWS_FORM_NO )) 
      *      AND ((@QWS_PURGE_TYPE              = 'A' ) 
      *       OR  (@QWS_PURGE_TYPE              = 'P'
      *      AND    FMFILE.FM_PRINT             = 'Y' )
      *       OR  (@QWS_PURGE_TYPE              = 'N'
      *      AND    FMFILE.FM_PRINT             = ' ' ));
      *
      *    ORDER BY FMFILE.FM_BRNO,
      *             FMFILE.FM_INITKEY,
      *             FMFILE.FM_FMNO;

           MOVE BEG-BR      TO QFM1-WBEG-BRNO.
           MOVE END-BR      TO QFM1-WEND-BRNO.
           MOVE FORM-NO     TO QWS-FORM-NO.
           MOVE PURGE-TYPE  TO QWS-PURGE-TYPE

           EXEC SQL SET CONNECTION FM1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE FM1_CURSOR CURSOR FOR
                :FM1-RETURN-CODE = EXEC SSP_FDPURG_FMFILE_SELECT 
              ( :QFM1-WBEG-BRNO,  
                :QFM1-WEND-BRNO,  
                :QWS-FORM-NO,  
                :QWS-PURGE-TYPE )      
           END-EXEC.

           EXEC SQL
               OPEN FM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QFM1-WBEG-BRNO 
                          QFM1-WEND-BRNO
                          QFM1-WBEG-FMNO
                          QFM1-WEND-FMNO.
           MOVE SPACES TO QFM1-WBEG-INITKEY
                          QFM1-WEND-INITKEY.
                            

           MOVE STAT---GOOD TO FM1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-FM1-FILE-NEXT. *> EMBEDDED - FMFILE
           PERFORM READ-IT.
           MOVE FM-PATH-OWNBR  TO FM-BRNO.
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE FM1-KEY        TO E-KEYX.
           INITIALIZE QFM-REC.
           INITIALIZE QFD-REC.

           EXEC SQL SET CONNECTION FM1_CONNECT END-EXEC.

           IF ( FM1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH FM1_CURSOR 
                  INTO 
                    :QFM-BRNO,
                    :QFM-INITKEY,
                    :QFM-FMNO,
                    :QFM-PRINT, 
                    :QFD-FMNO, 
                    :QFD-PROG 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-FM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-FM-FIELDS
              PERFORM GET-FD-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       DELETE-FM1-FILE. *> EMBEDDED - FMFILE

           MOVE FM-PATH-OWNBR  TO FM-BRNO.
           PERFORM DELETE-IT.
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE FM1-KEY        TO E-KEYX.
           PERFORM SET-FM-FIELDS.

           MOVE FM-BRNO     TO QFM-BRNO.
           MOVE FM-INITKEY  TO QFM-INITKEY.
           MOVE FM-FMNO     TO QFM-FMNO.

      * USE UPDATE_CONNECT
           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. 
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.FMFILE
             WHERE FMFILE.FM_BRNO    = :QFM-BRNO
               AND FMFILE.FM_INITKEY = :QFM-INITKEY
               AND FMFILE.FM_FMNO    = :QFM-FMNO
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-FM1-FILE.

      * USE C1 CONNECT
           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-FM1-FILE.

           PERFORM CLOSE-IT.

           IF ( FM1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION FM1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE FM1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO FM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-FM1-CONNECTION.

           IF FM1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION FM1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT FM1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO FM1-CONN-OPEN-SW.

      *-----------------------------------------------------------------

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **************************************
       END-PROGRAM SECTION.
      **************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY FDPURG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
