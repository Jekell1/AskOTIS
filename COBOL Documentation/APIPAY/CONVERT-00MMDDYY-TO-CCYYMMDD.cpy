      ***************************************************************
      * NAME: CONVERT-00MMDDYY-TO-CCYYMMDD
      * DESC: CONVERT A DATE IN THE FORMATE OF 00MMDDYY TO CCYYMMDD
      * IN  : CONVERT-DATE (00MMDDYY WHERE CONVERT-DATE NOT = 0)
      * OUT : CONVERT-DATE (CCYYMMDD)
      ***************************************************************
       CONVERT-00MMDDYY-TO-CCYYMMDD SECTION.
           IF CONVERT-DATE-CC = 00 AND CONVERT-DATE NOT = 0
              MOVE CONVERT-DATE-DD TO CONVERT-DATE-CC
              MOVE CONVERT-DATE-MM TO CONVERT-DATE-DD
              MOVE CONVERT-DATE-YY TO CONVERT-DATE-MM
              MOVE CONVERT-DATE-CC TO CONVERT-DATE-YY
              IF CONVERT-DATE-YY < EXT-JULIAN-CC
                 MOVE 20 TO CONVERT-DATE-CC
              ELSE
                 MOVE 19 TO CONVERT-DATE-CC
              END-IF
           END-IF.

      ***************************************************************
      * NAME: CONVERT-CCYYMMDD-TO-00MMDDYY
      * DESC: CONVERT A DATE IN THE FORMATE OF CCYYMMDD TO 00MMDDYY
      * IN  : CONVERT-DATE (CCYYMMDD)
      * OUT : CONVERT-DATE (00MMDDYY)
      ***************************************************************
