      * COPYMEMBER: LIBLP/LP01BYA
      ******************************************************************
      *
      * DESCRIPTION: BRANCH STATISTICS FILE - A (PRIMARY) AREA
      *
      * COPY USAGE : LIBLP/LPFDBYA.CPY
      *
      * FILE KEY   : #1 = DATE-CCYYMM, BRNO, CLASS
      *
      * FILE PATH  : /USR/DATA/R#/LP/BYAFILE
      *
      * ***ALL*** REVISION NOTES FOR FIELDS ARE LOCATED IN LP01BY.CPY
      *
      *=================================================================
      * REV:
      * KEC 190205 FOR A32, THE BYFILE WAS SPLIT INTO TWO FILES:
      *              BYAFILE (PRIMARY) AND BYBFILE (SECONDARY).
      *            LP01BY.CPY IS USED AS A MASTER LAYOUT (ALL LAYOUTS
      *            MERGED INTO ONE).
      *            LP01BYA/B.CPY IS FOR THE PRIMARY KEY FIELDS.
      *            LP01BYA/B_AREA.CPY IS FOR NON-KEY FIELDS.
      * KEC 190918 PT#0181 DUE TO ACU4GL THINKING THE BYA-REC IS ABOVE
      *            SQL LIMITS (1024 COLUMNS, 8064 BYTE ROWS), I MOVED
      *            THE FUTURE AMOUNT FIELDS FROM THE BYA TO THE BYB TO
      *            LOWER THE LIMITS SO THAT ACU4GL ACCEPTS IT.
      *              WAS: BYA-SIZE   VALUE 4466.
      *              NOW: BYA-SIZE   VALUE 3702.
      ******************************************************************
       78  BYA-SIZE                    VALUE 3702.
       01  BYA-REC.
           03  BYA1-KEY.
               05  BYA-DATE-CCYYMM     PIC 9(6).
               05  BYA-BRNO            PIC 9(4).
               05  BYA-CLASS           PIC 9(3).
       COPY "LIBLP/LP01BYA_AREA.CPY".
