      * FIELD SELECTION (FLDSEL)
      *************************************************************************
      * CALL-FLDSEL SECTION REPLACES "C" CALL FLDSEL USING.....
      *
      *  INPUT : FLDSEL-LIST   (ASSUME FIELDS ARE 010, 020, ...080
      *          DISPLAY-BUF   : ACTUAL DATA TO DISPLAY
      *                          DISPLAY-BUF CANNOT HAVE FILLER PADDING
      *                          PRIOR TO THE KEY FIELD FOR THE SCAN TO WORK
      *          FIRST-FIELD   : FIRST FIELD IN THE LIST (USUALLY 010)
      *          LAST-FIELD    : LAST FIELD IN THE LIST (THIS VARIES IN THE F10
      *                          BUT SHOULD BE STATIC IN MOST SCANS.
      *          FLDSEL-STAT   : NOT USED BUT APPEARED IN ORIG CALL,
      *                          MY BE UTILIZED AS WE CONVERT MORE
      *
      *          NOTE A CALL IS MADE TO VDU-CONVERT-FIELD
      *                (THIS SHOULD HAVE BEEN ADDED AS PART OF A30 CONVERSION
      *                 THE PLACE WHERE XXXXXX.EVA IS COPIED IN)
      *
      *          ADDITIONAL PARAMETERS:
      *          FSEL-SEARCH-TYPE:
      *                  ' ' DENOTES DEFAULT SEARCH.
      *                      ONLY LOOK AT 1ST CHAR IN COLUMN 1.
      *                  'L' FULL LINE SEARCH. LOOK FOR
      *                      FOR FIRST OCCURRENCE OF SEARCH CHARACTER ANYWHERE
      *                      IN THE DISPLAY LINE.
      *                      IF YOU WANT TO LIMIT THE COLUMNS WITHIN THE LINE
      *                      TO SEARCH SPECIFY THE RANGE WITH THE FOLLOWING:
      *                          FSEL-SEARCH-COL-BEGIN  99
      *                          FSEL-SEARCH-COL-END    99
      *                      I.E. USE THIS WHEN YOU WANT TO SEARCH FOR THE
      *                      OCCURRENCE OF THE SEARCH CHARACTER ANYWHERE IN THE
      *                      FIRST 3 COLUMNS OF THE DISPLAY LINE.
      *
      * REQUIRES LIBGB/SCREEN(W)
      *          LIBGB/FLDSELW
      *
      * OUTPUT: FLDSEL-STAT   : SAME AS FROM OLD FLDSEL OK, PG UP/DN, F7, ETC.
      *         FLDSEL-BUF    : COPY OF LINE SELECTED
      *         FLDSEL-FIELD  : NOT CURRENTLY USED BUT WE MAY FIND A PLACE
      *                         WHERE IT NEEDS TO BE (IS WAS PART OF ORIG CALL)
      *                       : CURRENT SUBSCRIPT OF LINE SELETED, SO YOU CAN
      *                         POINT TO A SEPERATE KEY TABLE
      *
      * USEAGE:  MAKE SURE DISPLAY-BUF IS SET UP AS BEFORE.
      *
      * REPLACE:
      *    CALL FLDSEL USING FLDSEL-STAT FLDSEL-LIST
      *                      FLDSEL-BUF  FLDSEL-FIELD.
      *
      * WITH
      *    PERFORM CALL-FLDSEL.
      *
      *  REV:
      *  MJD 130612 CHANGED TO MOVE PG DOWN TO STATUS AND RETURN IF YOU
      *             DOWN ARROW FROM THE END OF THE PAGE.  ALSO ADDED TEST
      *             TO STOP LOOPING WHEN YOU HIT A ROWD (D-LINE) OF SPACES.
      * BAH 131218 ADDED SET OF FLDSEL-FIELD TO THE SUBSCRIPT OF THE LINE
      *            SELECTED. SOME PROGRAMS HAVE A SEPARATE KEY TABLE THAT
      *            NEED THIS SUBSCRIPT.
      * MJD 140812 ADDED LOGIC TO USE FLDSEL-FIELD AS INPUT TO START
      *            AT A SPECIFIC LINE.
      * BLM 141126 ADDED LOGIC TO CALL HELP IF F8 IS PRESSED
      * BAH 20240308 REMOVED FSEL-STAT
      *************************************************************************
       CALL-FLDSEL SECTION.
       BEGIN-FLDSEL.
      *------------------------------------------------------------------
      * SETUP BEGINNING AND ENDING DISPLAY RANGES.
      *------------------------------------------------------------------
           MOVE FIRST-FIELD TO FSEL-FLD-ALPX.
           PERFORM FSEL-CONV-FLD-2-NUM.
           MOVE FSEL-FLD-NUM TO FSEL-BEG.
           IF FSEL-FLD-NUM = 99
              MOVE "99" TO FLDSEL-STAT
              GO TO FSEL-EXIT.

           MOVE LAST-FIELD TO FSEL-FLD-ALPX.
           PERFORM FSEL-CONV-FLD-2-NUM.
           MOVE FSEL-FLD-NUM TO FSEL-END.
           IF FSEL-FLD-NUM = 99
              MOVE "99" TO FLDSEL-STAT
              GO TO FSEL-EXIT.


      * ADDED THIS TO FORCE FLDSEL TO START AT A SPECIFIC LINE.
           MOVE FLDSEL-FIELD TO FSEL-FIELD
           MOVE FSEL-NO TO FSEL-CURR.

      *------------------------------------------------------------------
      *    MOVE FLDSEL-LIST TO AN OUTPUT LIST WHERE WE CAN MANIPULATE THE
      *    DISPLAY-ATTRIBUTES.
      *------------------------------------------------------------------
           MOVE FLDSEL-LIST TO FSEL-LIST-IN
           MOVE SPACES TO FSEL-LIST-OUT.


       FSEL-LOOP.
      * -------------------------------------------------------------------
      * TAKE THE DISPLAY LIST AND CONVERT IT TO ANOTHER LIST THAT INCLUDES
      * ":B" AT THE END TO FORMAT EACH FIELD
      *
      * THEN CHANGE THE FORMAT OF THE CURRENT LINE AND MAKE IT REVERSE (":R")
      * CHANGING THE CURRENT LINE TO REVERSE AND REDISPLAYING AS THE USER
      * UP/DOWN ARROWS THROUGH THE SCAN/LIST
      * -------------------------------------------------------------------
           MOVE SPACES TO FSEL-LIST-OUT.
           PERFORM VARYING FSEL-LINE-IDX FROM 1 BY 1
                               UNTIL FSEL-LINE-IDX > FSEL-END
                MOVE FSEL-FLD-IN(FSEL-LINE-IDX) TO
                                   FSEL-FLD-OUT(FSEL-LINE-IDX)
                MOVE ":" TO FSEL-COLON-OUT(FSEL-LINE-IDX)
                MOVE "B" TO FSEL-ATTR-OUT(FSEL-LINE-IDX)
                MOVE " " TO FSEL-SPACE-OUT(FSEL-LINE-IDX)
           END-PERFORM.
           MOVE "R" TO FSEL-ATTR-OUT(FSEL-CURR).
           MOVE "." TO FSEL-TERM-OUT.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE FSEL-LIST-OUT TO WR-LIST.
           PERFORM CALL-WRFORM.

      *------------------------------------------------------------------
      *   GET SCREEN POSITION FOR CURRENT FIELD (I.E. "010.") SO THA
      *   ACCEPT PROMPT CAN FOLLOW CURRENT LINE AND NOT SHOW SOMEWHERE
      *   ELSE ON THE WINDOW.
      *   SHIFT COLUMN 1 TO THE LEFT SO IT DOES NOT APPEAR OVER THE TEXT.
      *------------------------------------------------------------------
           MOVE SPACES TO VDU-FIELD-TO-CONVERT.
           STRING FSEL-FLD-IN(FSEL-CURR) "."
                  DELIMITED BY SPACES
                  INTO VDU-FIELD-TO-CONVERT.
           PERFORM VDU-CONVERT-FIELD.
           MOVE VDU-POSX TO VDU-POS.
           MOVE VDU-COL TO FSEL-ACCEPT-COL.
           MOVE VDU-LINE TO FSEL-ACCEPT-LINE.
           IF FSEL-ACCEPT-COL > 1
              SUBTRACT 1 FROM FSEL-ACCEPT-COL.

           MOVE SPACES TO FSEL-CHAR.

           ACCEPT FSEL-CHAR AT COLUMN FSEL-ACCEPT-COL
                               LINE FSEL-ACCEPT-LINE
                               NO ECHO
                               AUTO
                               UPPER
               ON EXCEPTION VDU-KEY-DEST
                            PERFORM VDU-SET-STATUS
           END-ACCEPT.

           MOVE STAT TO VDUSTATUS FLDSEL-STAT.
           MOVE D-LINE(FSEL-CURR) TO FLDSEL-BUF.
           MOVE FSEL-CURR TO FLDSEL-FIELD.

           IF F8-KEY
              IF HELP-LINK-FILE = SPACES
                 MOVE VDU-FORM-NAME TO HELP-LINK-FILE
              END-IF
              MOVE FSEL-FLD-ALP TO HELP-LINK-FIELD
              INSPECT HELP-LINK-FIELD REPLACING ALL "." BY " "
              PERFORM SETUP-LINK-INFO
              MOVE "GB/HELP" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATHNAME HELP-LINK-BUF
                                   EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO BEGIN-FLDSEL
           END-IF.

           IF F1-KEY OR F2-KEY OR F3-KEY OR F4-KEY OR F5-KEY OR
              F6-KEY OR F9-KEY OR F10-KEY OR
              F11-KEY OR F12-KEY OR HOME-KEY OR LEFT-KEY OR RIGHT-KEY
                 GO TO FSEL-EXIT.

           IF PAGE-UP-KEY OR PAGE-DOWN-KEY OR F7-KEY
              MOVE 1 TO FSEL-CURR
              GO TO FSEL-EXIT.
  
           IF ENTER-KEY AND FSEL-CHAR = " "
              GO TO FSEL-EXIT.

           IF UP-KEY
              IF FSEL-CURR = 1
                 MOVE 1 TO FSEL-CURR
                 GO TO FSEL-EXIT
              ELSE
                 SUBTRACT 1 FROM FSEL-CURR
                 GO TO FSEL-LOOP.

           IF DOWN-KEY
              IF FSEL-CURR = FSEL-END
                 MOVE VDU---PAGE-DOWN-KEY TO FLDSEL-STAT
                 MOVE 1 TO FSEL-CURR
                 GO TO FSEL-EXIT
              ELSE
                 ADD 1 TO FSEL-CURR
                 IF FSEL-CURR > FSEL-END
                    MOVE VDU---PAGE-DOWN-KEY TO FLDSEL-STAT
                    MOVE 1 TO FSEL-CURR
                    GO TO FSEL-EXIT
                 END-IF
                 IF D-LINE(FSEL-CURR) = SPACES
                    SUBTRACT 1 FROM FSEL-CURR
                 END-IF
                 GO TO FSEL-LOOP.

           PERFORM FSEL-FIND-KEY.
           GO TO FSEL-LOOP.

       FSEL-EXIT.
           EXIT.

      ************************************************************************
      * SEARCH FLDSEL BASED ON 1ST CHAR OF KEY
      *
      * COMPARE THE SEARCH KEY TO THE FIRST CHARACTER OF THE D-KEY TO
      * SET THE CURRENT LINE TO THAT VALUE (I.E. IN F10 USER HITS "S" TO
      * JUMP TO "SALES...."
      *
      * STARTED INNER LOOP TO SCAN ENTIRE LINE BUT DID NOT FINISH IT.
      * LEFT LOGIC HERE IN CASE WE DECIDE TO PICK IT BACK UP LATER.
      ************************************************************************
       FSEL-FIND-KEY SECTION.
           MOVE "N" TO FSEL-MATCH.
           
      * IF SEARCHING LINE ALWAYS START SEARCHING FROM COLUMN 1 OF EACH
      *     NEW LINE UNLESS SERCHING WITH A SPECIFIED RANGE OF COLUMNS.
      * BY DEFAULT ALWAYS START AT COLUMN 1
           IF FSEL-SEARCH-LINE
              IF NOT FSEL-SEARCH-COL-BEGIN-VALID
                 MOVE 1 TO FSEL-SEARCH-COL-BEGIN
              END-IF
              IF NOT FSEL-SEARCH-COL-END-VALID
                 MOVE 80 TO FSEL-SEARCH-COL-END
              END-IF
              IF FSEL-SEARCH-COL-BEGIN > FSEL-SEARCH-COL-END
                 MOVE 1 TO FSEL-SEARCH-COL-BEGIN
                 MOVE 80 TO FSEL-SEARCH-COL-END
              END-IF
              MOVE FSEL-SEARCH-COL-BEGIN TO FSEL-COL
           ELSE
              MOVE 1 TO FSEL-COL
           END-IF.

      * ALWAYS START SEARCHING LINE UNDER CURRENT POSITION.
      * IF AT END RESET TO 1ST LINE

           MOVE FSEL-CURR TO FSEL-HOLD-LINE.
           ADD 1 TO FSEL-CURR GIVING FSEL-START-LINE.
           IF FSEL-START-LINE > FSEL-END
              MOVE 1 TO FSEL-START-LINE
           END-IF.

       FSEL-FIND-KEY-LINE-LOOP.
           PERFORM VARYING FSEL-LINE-IDX FROM FSEL-START-LINE BY 1
                               UNTIL (FSEL-LINE-IDX > FSEL-END OR
                                           FSEL-MATCH-FOUND)
              MOVE D-LINE(FSEL-LINE-IDX) TO FSEL-D-LINE
              MOVE 0 TO SUB
              INSPECT FSEL-D-LINE TALLYING SUB FOR TRAILING " "
              SUBTRACT SUB FROM 81 GIVING FSEL-EOL
              
              IF FSEL-SEARCH-LINE
                 IF FSEL-EOL > FSEL-SEARCH-COL-END
                    MOVE FSEL-SEARCH-COL-END TO FSEL-EOL
                 END-IF
                 MOVE FSEL-SEARCH-COL-BEGIN TO FSEL-COL
                 IF FSEL-COL > FSEL-EOL
                    MOVE 1 TO FSEL-COL
                 END-IF
                 PERFORM VARYING FSEL-COL-IDX FROM FSEL-COL BY 1
                                    UNTIL (FSEL-COL-IDX > FSEL-EOL OR
                                           FSEL-MATCH-FOUND)
                    IF FSEL-D-CHAR(FSEL-COL-IDX) = FSEL-CHAR
                       MOVE FSEL-LINE-IDX TO FSEL-CURR
                       MOVE "Y" TO FSEL-MATCH
                    END-IF
                 END-PERFORM
              ELSE
                 IF FSEL-D-CHAR(1) = FSEL-CHAR
                    MOVE FSEL-LINE-IDX TO FSEL-CURR
                    MOVE "Y" TO FSEL-MATCH
                 END-IF
              END-IF
           END-PERFORM.
           
      * IF WE HIT HIT THE BOTTOM OF THE LIST WITH NO MATCH, 
      * AND WE STARTED THE SEARCH FOMR A POSITION OTHER THAN THE FIRST LINE
      * THEN RESET POSITION TO TOP OF LIST AND SEARCH AGAIN

           IF NOT FSEL-MATCH-FOUND
              IF FSEL-HOLD-LINE NOT = 1
                 MOVE 1 TO FSEL-START-LINE FSEL-HOLD-LINE
                 IF FSEL-SEARCH-LINE
                    ADD 1 TO FSEL-COL
                 END-IF
                 GO TO FSEL-FIND-KEY-LINE-LOOP
              END-IF
           END-IF.

      ************************************************************************
      * CONVERT SCREEN FIELD NAME (ALPHA) TO NUMERIC
      *  USED INTERNALLY  IN CALL-FLDSEL.
      *
      * USED TO GET BEG AND ENDING RANGES FOR LOOPING BASED ON
      *    FIRST-FIELD, LAST-FIELD
      ************************************************************************
       FSEL-CONV-FLD-2-NUM SECTION.
           EVALUATE FSEL-FLD-ALP
               WHEN "010" MOVE  1 TO FSEL-FLD-NUM
               WHEN "020" MOVE  2 TO FSEL-FLD-NUM
               WHEN "030" MOVE  3 TO FSEL-FLD-NUM
               WHEN "040" MOVE  4 TO FSEL-FLD-NUM
               WHEN "050" MOVE  5 TO FSEL-FLD-NUM
               WHEN "060" MOVE  6 TO FSEL-FLD-NUM
               WHEN "070" MOVE  7 TO FSEL-FLD-NUM
               WHEN "080" MOVE  8 TO FSEL-FLD-NUM
               WHEN "090" MOVE  9 TO FSEL-FLD-NUM
               WHEN "100" MOVE 10 TO FSEL-FLD-NUM
               WHEN "110" MOVE 11 TO FSEL-FLD-NUM
               WHEN "120" MOVE 12 TO FSEL-FLD-NUM
               WHEN "130" MOVE 13 TO FSEL-FLD-NUM
               WHEN "140" MOVE 14 TO FSEL-FLD-NUM
               WHEN "150" MOVE 15 TO FSEL-FLD-NUM
               WHEN "160" MOVE 16 TO FSEL-FLD-NUM
               WHEN "170" MOVE 17 TO FSEL-FLD-NUM
               WHEN "180" MOVE 18 TO FSEL-FLD-NUM
               WHEN "190" MOVE 19 TO FSEL-FLD-NUM
               WHEN "200" MOVE 20 TO FSEL-FLD-NUM
               WHEN "210" MOVE 21 TO FSEL-FLD-NUM
               WHEN "220" MOVE 22 TO FSEL-FLD-NUM
               WHEN "230" MOVE 23 TO FSEL-FLD-NUM
               WHEN "240" MOVE 24 TO FSEL-FLD-NUM
               WHEN "250" MOVE 25 TO FSEL-FLD-NUM
               WHEN "260" MOVE 26 TO FSEL-FLD-NUM
               WHEN "270" MOVE 27 TO FSEL-FLD-NUM
               WHEN "280" MOVE 28 TO FSEL-FLD-NUM
               WHEN "290" MOVE 29 TO FSEL-FLD-NUM
               WHEN "300" MOVE 10 TO FSEL-FLD-NUM
               WHEN "310" MOVE 31 TO FSEL-FLD-NUM
               WHEN "320" MOVE 32 TO FSEL-FLD-NUM
               WHEN "330" MOVE 33 TO FSEL-FLD-NUM
               WHEN "340" MOVE 34 TO FSEL-FLD-NUM
               WHEN "350" MOVE 35 TO FSEL-FLD-NUM
               WHEN "360" MOVE 36 TO FSEL-FLD-NUM
               WHEN "370" MOVE 37 TO FSEL-FLD-NUM
               WHEN "380" MOVE 38 TO FSEL-FLD-NUM
               WHEN "390" MOVE 39 TO FSEL-FLD-NUM
               WHEN "400" MOVE 40 TO FSEL-FLD-NUM
               WHEN "410" MOVE 41 TO FSEL-FLD-NUM
               WHEN "420" MOVE 42 TO FSEL-FLD-NUM
               WHEN "430" MOVE 43 TO FSEL-FLD-NUM
               WHEN "440" MOVE 44 TO FSEL-FLD-NUM
               WHEN "450" MOVE 45 TO FSEL-FLD-NUM
               WHEN "460" MOVE 46 TO FSEL-FLD-NUM
               WHEN "470" MOVE 47 TO FSEL-FLD-NUM
               WHEN "480" MOVE 48 TO FSEL-FLD-NUM
               WHEN "490" MOVE 49 TO FSEL-FLD-NUM
               WHEN "500" MOVE 50 TO FSEL-FLD-NUM
               WHEN "510" MOVE 51 TO FSEL-FLD-NUM
               WHEN "520" MOVE 52 TO FSEL-FLD-NUM
               WHEN "530" MOVE 53 TO FSEL-FLD-NUM
               WHEN "540" MOVE 54 TO FSEL-FLD-NUM
               WHEN "550" MOVE 55 TO FSEL-FLD-NUM
               WHEN "560" MOVE 56 TO FSEL-FLD-NUM
               WHEN "570" MOVE 57 TO FSEL-FLD-NUM
               WHEN "580" MOVE 58 TO FSEL-FLD-NUM
               WHEN "590" MOVE 59 TO FSEL-FLD-NUM
               WHEN "600" MOVE 60 TO FSEL-FLD-NUM
               WHEN "610" MOVE 61 TO FSEL-FLD-NUM
               WHEN "620" MOVE 62 TO FSEL-FLD-NUM
               WHEN "630" MOVE 63 TO FSEL-FLD-NUM
               WHEN "640" MOVE 64 TO FSEL-FLD-NUM
               WHEN "650" MOVE 65 TO FSEL-FLD-NUM
               WHEN "660" MOVE 66 TO FSEL-FLD-NUM
               WHEN "670" MOVE 67 TO FSEL-FLD-NUM
               WHEN "680" MOVE 68 TO FSEL-FLD-NUM
               WHEN "690" MOVE 69 TO FSEL-FLD-NUM
               WHEN "700" MOVE 70 TO FSEL-FLD-NUM
               WHEN "710" MOVE 71 TO FSEL-FLD-NUM
               WHEN "720" MOVE 72 TO FSEL-FLD-NUM
               WHEN "730" MOVE 73 TO FSEL-FLD-NUM
               WHEN "740" MOVE 74 TO FSEL-FLD-NUM
               WHEN "750" MOVE 75 TO FSEL-FLD-NUM
               WHEN "760" MOVE 76 TO FSEL-FLD-NUM
               WHEN "770" MOVE 77 TO FSEL-FLD-NUM
               WHEN "780" MOVE 78 TO FSEL-FLD-NUM
               WHEN "790" MOVE 79 TO FSEL-FLD-NUM
               WHEN "800" MOVE 80 TO FSEL-FLD-NUM
               WHEN OTHER MOVE 99 TO FSEL-FLD-NUM
           END-EVALUATE.
