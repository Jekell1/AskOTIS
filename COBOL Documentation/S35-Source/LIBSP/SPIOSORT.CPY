      * COPYMEMBER: LIBSP/SPIOSORT
      ******************************************************************
      *
      * IDENTIFICATION:  LIBSP/SPIOSORT
      *
      * DESCRIPTION   :  STANDARD SORT FILE (IO)
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
       LOAD-SORT-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO SORT-PATH.
           PERFORM OPEN-SORT-FILE-OUTPUT.
           PERFORM CLOSE-SORT-FILE.

      *-----------------------------------------------------------------
       OPEN-SORT-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE SORT-IFN TO E-FILE.
           OPEN OUTPUT SORT-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-SORT-FILE-OUTPUT.

           IF ( IO-ALREADY-OPEN )
              CLOSE SORT-FILE
              GO TO OPEN-SORT-FILE-OUTPUT.

           UNLOCK SORT-FILE.

      *-----------------------------------------------------------------
       OPEN-SORT-FILE.

           PERFORM OPEN-IT.
           MOVE SORT-IFN TO E-FILE.
           OPEN I-O SORT-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-SORT-FILE.

           IF ( IO-ALREADY-OPEN )
              CLOSE SORT-FILE
              GO TO OPEN-SORT-FILE.

           UNLOCK SORT-FILE.

      *-----------------------------------------------------------------
       READ-SORT-FILE.

           PERFORM READ-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-KEY TO E-KEYX.
           READ SORT-FILE.

           IF ( IO-LOCKED )
              GO TO READ-SORT-FILE.

      *-----------------------------------------------------------------
       READ-SORT-FILE-NEXT.

           PERFORM READ-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-KEY TO E-KEYX.
           READ SORT-FILE NEXT.

           IF ( IO-LOCKED )
              GO TO READ-SORT-FILE-NEXT.

      *-----------------------------------------------------------------
       START-SORT-FILE.

           PERFORM START-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-KEY TO E-KEYX.
           START SORT-FILE KEY NOT < SORT-KEY.
           UNLOCK SORT-FILE.

      *-----------------------------------------------------------------
       WRITE-SORT-FILE.

           PERFORM WRITE-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-KEY TO E-KEYX.

           WRITE SORT-REC.

           IF ( IO-LOCKED )
             GO TO WRITE-SORT-FILE.

           UNLOCK SORT-FILE.

      *-----------------------------------------------------------------
       REWRITE-SORT-FILE.

           PERFORM REWRITE-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-KEY TO E-KEYX.
           REWRITE SORT-REC.

           IF ( IO-LOCKED )
              GO TO REWRITE-SORT-FILE.

           UNLOCK SORT-FILE.

      *-----------------------------------------------------------------
       CLOSE-SORT-FILE.

           PERFORM CLOSE-IT.
           CLOSE SORT-FILE.

      *-----------------------------------------------------------------
       REMOVE-SORT-FILE.

           MOVE SORT-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
