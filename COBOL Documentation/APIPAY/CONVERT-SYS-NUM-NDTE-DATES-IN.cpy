      ***************************************************************
      * NAME: CONVERT-SYS-NUM-NDTE-DATES-IN
      * DESC: CONVERT SYS/NUM/NDTE-DATE DATE FROM 00MMDDYY TO CCYYMMDD
      *       USED STRICTLY FOR TESTING WHILE INCEMENTALLY CHANING DATE
      *       FIELDS AND FORMATS.
      *       ALLOWS INTERNAL USAGE OF CCYYMMDD WHILE EXTERNAL FORMAT
      *       CAN BE CCYYMMDD OR 00MMDDYY
      * IN  : SYS-DATE (00MMDDYY)
      *       NUM-DATE (00MMDDYY)
      *       NDTE-DATE (00MMDDYY)
      * OUT : SYS-DATE (CCYYMMDD)
      *       NUM-DATE (CCYYMMDD)
      *       NDTE-DATE (CCYYMMDD)
      ***************************************************************
       CONVERT-SYS-NUM-NDTE-DATES-IN SECTION.
           MOVE "N" TO SYS-FG NUM-FG NDTE-FG.
           IF S-CC = 0 AND SYS-DATE NOT = 0
              MOVE SYS-DATE TO CONVERT-DATE
              PERFORM CONVERT-00MMDDYY-TO-CCYYMMDD
              MOVE CONVERT-DATE TO SYS-DATE
              MOVE "Y" TO SYS-FG.

           IF NUM-CC = 0 AND NUM-DATE NOT = 0
              MOVE NUM-DATE TO CONVERT-DATE
              PERFORM CONVERT-00MMDDYY-TO-CCYYMMDD
              MOVE CONVERT-DATE TO NUM-DATE
              MOVE "Y" TO NUM-FG.

           IF NDTE-CC = 0 AND NDTE-DATE NOT = 0
              MOVE NDTE-DATE TO CONVERT-DATE
              PERFORM CONVERT-00MMDDYY-TO-CCYYMMDD
              MOVE CONVERT-DATE TO NDTE-DATE
              MOVE "Y" TO NDTE-FG.

      ***************************************************************
      * NAME: CONVERT-SYS-NUM-NDTE-DATES-OUT
      * DESC: CONVERT SYS/NUM/NDTE-DATE DATE FROM 00MMDDYY TO CCYYMMDD
      *       USED STRICTLY FOR TESTING WHILE INCEMENTALLY CHANING DATE
      *       FIELDS AND FORMATS.
      *       ALLOWS INTERNAL USAGE OF CCYYMMDD WHILE EXTERNAL FORMAT
      *       CAN BE CCYYMMDD OR 00MMDDYY
      * IN  : SYS-DATE (CCYYMMDD)
      *       NUM-DATE (CCYYMMDD)
      *       NDTE-DATE (CCYYMMDD)
      * OUT : SYS-DATE (00MMDDYY)
      *       NUM-DATE (00MMDDYY)
      *       NDTE-DATE (00MMDDYY)
      ***************************************************************
