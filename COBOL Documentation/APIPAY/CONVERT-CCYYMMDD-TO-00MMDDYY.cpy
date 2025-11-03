      ***************************************************************
      * NAME: CONVERT-CCYYMMDD-TO-00MMDDYY
      * DESC: CONVERT A DATE IN THE FORMATE OF CCYYMMDD TO 00MMDDYY
      * IN  : CONVERT-DATE (CCYYMMDD)
      * OUT : CONVERT-DATE (00MMDDYY)
      ***************************************************************
       CONVERT-CCYYMMDD-TO-00MMDDYY SECTION.
           MOVE CONVERT-DATE-YY TO CONVERT-DATE-CC.
           MOVE CONVERT-DATE-MM TO CONVERT-DATE-YY.
           MOVE CONVERT-DATE-DD TO CONVERT-DATE-MM.
           MOVE CONVERT-DATE-CC TO CONVERT-DATE-DD.
           MOVE 00              TO CONVERT-DATE-CC.
     

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
