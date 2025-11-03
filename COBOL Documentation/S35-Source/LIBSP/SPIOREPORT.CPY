      * COPYMEMBER: LIBSP/SPIOREPORT
      ******************************************************************
      *
      * IDENTIFICATION:  LIBSP/SPIOREPORT
      *
      * DESCRIPTION   :  STANDARD REPORT FILE (IO)
      *
      * *** NEEDED ***:  LIBGB/ACCESSW
      *                  LIBGB/RM.CMD
      *                  LIBGB/SYSTEMW
      *
      *                  LIBGB/ACCESS (PARAGRAPH; NOT A SECTION)
      *                  LIBGB/RMFILE (PARAGRAPH; NOT A SECTION)
      *                  LIBGB/SYSTEM (PARAGRAPH; NOT A SECTION)
      *
      *=================================================================
      * REV:
      * KEC 2003.1023 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               ***NEW***
      *
      ******************************************************************
       LOAD-REPORT-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO REPORT-PATH.
           PERFORM OPEN-REPORT-FILE-OUTPUT.
           PERFORM CLOSE-REPORT-FILE.

      *-----------------------------------------------------------------
       OPEN-REPORT-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE REPORT-IFN TO E-FILE.
           OPEN OUTPUT REPORT-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-REPORT-FILE-OUTPUT.

           IF ( IO-ALREADY-OPEN )
              CLOSE REPORT-FILE
              GO TO OPEN-REPORT-FILE-OUTPUT.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       OPEN-REPORT-FILE.

           PERFORM OPEN-IT.
           MOVE REPORT-IFN TO E-FILE.
           OPEN I-O REPORT-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-REPORT-FILE.

           IF ( IO-ALREADY-OPEN )
              CLOSE REPORT-FILE
              GO TO OPEN-REPORT-FILE.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       READ-REPORT-FILE.

           PERFORM READ-IT.
           MOVE REPORT-IFN TO E-FILE.
           MOVE REPORT-KEY TO E-KEYX.
           READ REPORT-FILE.

           IF ( IO-LOCKED )
              GO TO READ-REPORT-FILE.

      *-----------------------------------------------------------------
       READ-REPORT-FILE-NEXT.

           PERFORM READ-IT.
           MOVE REPORT-IFN TO E-FILE.
           MOVE REPORT-KEY TO E-KEYX.
           READ REPORT-FILE NEXT.

           IF ( IO-LOCKED )
              GO TO READ-REPORT-FILE-NEXT.

      *-----------------------------------------------------------------
       START-REPORT-FILE.

           PERFORM START-IT.
           MOVE REPORT-IFN TO E-FILE.
           MOVE REPORT-KEY TO E-KEYX.
           START REPORT-FILE KEY NOT < REPORT-KEY.
           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       WRITE-REPORT-FILE.

           PERFORM WRITE-IT.
           MOVE REPORT-IFN TO E-FILE.
           MOVE REPORT-KEY TO E-KEYX.

           WRITE REPORT-REC.

           IF ( IO-LOCKED )
             GO TO WRITE-REPORT-FILE.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       REWRITE-REPORT-FILE.

           PERFORM REWRITE-IT.
           MOVE REPORT-IFN TO E-FILE.
           MOVE REPORT-KEY TO E-KEYX.
           REWRITE REPORT-REC.

           IF ( IO-LOCKED )
              GO TO REWRITE-REPORT-FILE.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       CLOSE-REPORT-FILE.

           PERFORM CLOSE-IT.
           CLOSE REPORT-FILE.

      *-----------------------------------------------------------------
       REMOVE-REPORT-FILE.

           MOVE REPORT-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
