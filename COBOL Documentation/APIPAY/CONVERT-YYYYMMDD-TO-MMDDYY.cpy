      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'YYYYMMDD' TO 'MMDDYY'
      *   IN  :  DATE-YYYYMMDD
      *   OUT :  DATE-MMDDYY
      *-----------------------------------------------------------------
      * REV:
      * KEC 033099 ***NEW***
      ******************************************************************
       CONVERT-YYYYMMDD-TO-MMDDYY SECTION.

           MOVE DATE-YYYYMMDD-MM TO DATE-MMDDYY-MM.
           MOVE DATE-YYYYMMDD-DD TO DATE-MMDDYY-DD.
           MOVE DATE-YYYYMMDD-YY TO DATE-MMDDYY-YY.

       CV-YYYYMMDD-TO-MMDDYY-EXIT.
           EXIT.

      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'YYYYMMDD' TO 'MMDDYYYY'
      *   IN  :  DATE-YYYYMMDD
      *   OUT :  DATE-MMDDYYYY
      *-----------------------------------------------------------------
      * REV:
      *  BAH 10/20/2016  ***NEW***
      ******************************************************************
