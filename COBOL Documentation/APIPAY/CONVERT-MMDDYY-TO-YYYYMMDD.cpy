      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'MMDDYY' TO 'YYYYMMDD'
      *   IN  :  DATE-MMDDYY
      *   OUT :  DATE-YYYYMMDD
      *-----------------------------------------------------------------
      * REV:
      * KEC 033099 ***NEW***
      ******************************************************************
       CONVERT-MMDDYY-TO-YYYYMMDD SECTION.

           MOVE DATE-MMDDYY-MM TO DATE-YYYYMMDD-MM.
           MOVE DATE-MMDDYY-DD TO DATE-YYYYMMDD-DD.
           MOVE DATE-MMDDYY-YY TO DATE-YYYYMMDD-YYYY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  NOTE: IF DATE-MMDDYY IS SPACES THEN THE FOLLOWING WILL MAKE
      *        DATE-YYYYMMDD TO BE "2000    ".  MUST ADD NUMERIC CHECK
      *        IN GIVEN PROGRAM PRIOR TO PERFORMING THIS ROUTINE IF
      *        YOU KNOW THAT DATE-MMDDYY COULD BE SPACES/NON-NUMERIC.
      *        SEE THE NOTE AT THE TOP OF LIBGB/DATER FOR MORE INFO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( DATE-YYYYMMDD NOT = ZEROES )
              IF ( DATE-YYYYMMDD-YYYY < EXT-JULIAN-CC )
                 ADD 2000 TO DATE-YYYYMMDD-YYYY
              ELSE
                 ADD 1900 TO DATE-YYYYMMDD-YYYY.

       CV-MMDDYY-TO-YYYYMMDD-EXIT.
           EXIT.

      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'MMDDYY' TO 'YYYYMM'
      *   IN  :  DATE-MMDDYY
      *   OUT :  DATE-YYYYMM
      *-----------------------------------------------------------------
      * REV:
      * KEC 2000.0204 ***NEW***
      *
      ******************************************************************
