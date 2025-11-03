       identification division.
       program-id.    ac2day.
       author.         cs.
       date-written.  09-11-2017.
      ******************************************************************
      *
      * identification:  lp/ac2day.c
      *                 (taken from lp/ac1day)
      *
      * description   :  monthly earnings accrual
      *                  ib exceptions,precomp,precomp exceptions.
      *
      *                 runs in batch with ability to local spool
      *                 (sp/eoexec), has group logic but no group totals.
      *
      *                 print order for group from menu (lpr3mu)
      *                 ib for all branches, ib exceptions for all branches
      *                 precomp for all branches, precomp exceptions for
      *                 all branches if all loan type prints
      *                 requested.  world pr#1271
      * 
      * note: this program creates extracts of the detail reports
      *
      * explanation from cindy;
      *
      * separate sections for precomp and ib.
      * precomp uses the accrual date and computes earnings(lpaern)
      *   takes into account ln-erndelq-stop following all other parameters.
      *   skips p&l
      * ib coputes a max accrual based on sp-max-ib-acr-days. if this is 0,
      *   its done with this loan. also uses accrual date and set that as
      *   the rate date and performs set-py-rate.
      *   then generates the ibx setion which are ib exceptions - loans that
      *   the true accrue exceeds max ib interest.
      * all the above is then loaded into a report file only. no updates.
      *
      *=================================================================
      * rev:
      * bah 2017-0316 activate 8 digit dates, pd0006
      * bah 2017-0316 added ltr-rec to rvcalc linkage like ac1ern
      *  cs 2017-0912 accrual date was defaulting to 00/00/00 instead
      *            of system date. move accrual-date to
      *            vdu-date-yyyymmdd.
      * bah 2018-1002 global bwfile, l2g
      * bah 2018-1010 global dlfile, l2g
      * bah 2018-1203 global lnfile, l2g
      *  cs 2019-0103 global ltifile,l2g,
      *          comment lti-local-path/ltp-local-path
      *          ken and i couldnt find in use
      *  cs 2019-0111 global ltifile,l2g
      * kec 2019-0131 cc32 - removed the revolving credit usage.
      * bah 2019-0306 branch ranges were getting wiped out after running
      *          it in group batch, world pr#1362
      * bah 2019-0424 was printing lname, lname instead of fname, qa0010
      * bah 2019-0703 removed ltr pt0150
      * bah 2019-0922 removed access-call lnfile, bw, lti and lp
      * jkc 2020-1130 pr#1477 add flag to screen to create export file
      *          (y/n/o), default to "n".  depending on sections
      *          requested, up to 4 different data files will be created
      *          with account detail : ib accrual, ib exceptions,
      *          pre-compute accrual, pre-compute exceptions (currently
      *          run for pre-compute exceptions only).  add password
      *          sequence so that end-users cannot change the flag from
      *          "n", we will limit option to admin logins.
      *
      * jkc 2021-0111 pr#1477 in qa testing, it was found that the
      *          option of export file (o)nly did not work unlike the
      *          option to include the export file with the report.
      *          had to include additional logic to process doing an
      *          option of export file only right after the read-next
      *          of the report file and before the check if the record
      *          is a totals record and to skip if not printing those
      *          types of records.
      *
      * jkc 2021-0210 pr#1477a need additional information on each line.
      *          the export file records (lines) needs branch number,
      *          loan class, and split account number and status flags
      *          into seperate fields.  so lines began as:
      *             branch/account number/status flags/loan class
      *          replaced display-acctno and display-lnno with
      *          wss-lps-display-lnno which is a redefines for different
      *          formats needed in populating the export fields.
      *
      *
      * jkc 2021-0224 pr#1477a per email of andria sinclair and
      *          kevin mullen, add a space before ac2day-ib-max.
      *
      * jkc 2022-0809 pr#1565 added ln-actioncd2 to the end of all four
      *          extraction files.
      *
      ******************************************************************
       environment division.
       configuration section.
       source-computer. 3b.
       object-computer. 3b.
       input-output section.
       file-control.
       copy "libgb/gbfspr.cpy".


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           select ac2day-ib-file assign ac2day-ib-path
                  organization relative
                  access mode dynamic
                  lock mode automatic with lock on record
                  relative key ac2day-ib-key
                  file status file-stat.

           select ac2day-ibx-file assign ac2day-ibx-path
                  organization relative
                  access mode dynamic
                  lock mode automatic with lock on record
                  relative key ac2day-ibx-key
                  file status file-stat.

           select ac2day-pc-file assign ac2day-pc-path
                  organization relative
                  access mode dynamic
                  lock mode automatic with lock on record
                  relative key ac2day-pc-key
                  file status file-stat.

           select ac2day-pcx-file assign ac2day-pcx-path
                  organization relative
                  access mode dynamic
                  lock mode automatic with lock on record
                  relative key ac2day-pcx-key
                  file status file-stat.

           select report-file assign report-path
                  organization indexed
                  access dynamic
                  lock mode automatic with lock on record
                  record key report-key
                  file status file-stat.

      *-----------------------------------------------------------------
       i-o-control.
       data division.
       file section.
       copy "libgb/gbfdpr.cpy".


      ******************************************************************
      *
      *    a c 2 d a y - i b - f i l e
      *
      ******************************************************************
       fd  ac2day-ib-file
           label records are standard.
       01  ac2day-ib-rec.
           03  ac2day-ib-brno                    pic 9(4).
           03  ac2day-ib-acctno                  pic 9(6).
           03  ac2day-ib-7status                 pic x(7).
           03  ac2day-ib-class                   pic 9(2).
           03  ac2day-ib-name                    pic x(13).
           03  ac2day-ib-number-accts            pic 9(3).
           03  ac2day-ib-from-date               pic 9(8).
           03  ac2day-ib-int-since-lstpy         pic 9(7).99.
           03  ac2day-ib-intbal                  pic 9(7).99.
           03  ac2day-ib-true-accrue             pic 9(7).99.
           03  ac2day-ib-accrued                 pic 9(6).99.
           03  filler                            pic x(1).
           03  ac2day-ib-max                     pic x(1).
           03  ac2day-ib-actioncd2               pic x(4).
           03  ac2day-ib-cr                      pic x(1).
           03  ac2day-ib-lf                      pic x(1).

      ******************************************************************
      *
      *    a c 2 d a y - i b x - f i l e
      *
      ******************************************************************
       fd  ac2day-ibx-file
           label records are standard.
       01  ac2day-ibx-rec.
           03  ac2day-ibx-brno                   pic 9(4).
           03  ac2day-ibx-acctno                 pic 9(6).
           03  ac2day-ibx-7status                pic x(7).
           03  ac2day-ibx-class                  pic 9(2).
           03  ac2day-ibx-name                   pic x(13).
           03  ac2day-ibx-number-accts           pic 9(3).
           03  ac2day-ibx-int-pdth-date          pic 9(8).
           03  ac2day-ibx-int-since-lstpy        pic 9(7).99.
           03  ac2day-ibx-int-owed               pic 9(7).99.
           03  ac2day-ibx-true-accrue            pic 9(7).99.
           03  ac2day-ibx-income-accrue          pic 9(6).99.
           03  ac2day-ibx-balance                pic 9(7).99.
           03  ac2day-ibx-cont-pdth-date         pic 9(8).
           03  ac2day-ibx-cont-status            pic 9(3).
           03  ac2day-ibx-actioncd2              pic x(4).
           03  ac2day-ibx-cr                     pic x(1).
           03  ac2day-ibx-lf                     pic x(1).

      ******************************************************************
      *
      *    a c 2 d a y - p c - f i l e
      *
      ******************************************************************
       fd  ac2day-pc-file
           label records are standard.
       01  ac2day-pc-rec.
           03  ac2day-pc-brno                    pic 9(4).
           03  ac2day-pc-acctno                  pic 9(6).
           03  ac2day-pc-7status                 pic x(7).
           03  ac2day-pc-class                   pic 9(2).
           03  ac2day-pc-name                    pic x(13).
           03  ac2day-pc-anterm                  pic 9(3).
           03  ac2day-pc-antic                   pic +9(06).99.
           03  ac2day-pc-unearn                  pic +9(06).99.
           03  ac2day-pc-int                     pic +9(06).99.
           03  ac2day-pc-addon                   pic +9(06).99.
           03  ac2day-pc-srv                     pic +9(06).99.
           03  ac2day-pc-mfee                    pic +9(06).99.
           03  ac2day-pc-disc                    pic +9(06).99.
           03  ac2day-pc-other                   pic +9(06).99.
           03  ac2day-pc-cash-earn               pic +9(06).99.
           03  ac2day-pc-ending                  pic +9(06).99.
           03  ac2day-pc-actioncd2               pic x(4).
           03  ac2day-pc-cr                      pic x(1).
           03  ac2day-pc-lf                      pic x(1).

      ******************************************************************
      *
      *    a c 2 d a y - p c x - f i l e
      *
      ******************************************************************
       fd  ac2day-pcx-file
           label records are standard.
       01  ac2day-pcx-rec.
           03  ac2day-pcx-brno                   pic 9(4).
           03  ac2day-pcx-acctno                 pic 9(6).
           03  ac2day-pcx-7status                pic x(7).
           03  ac2day-pcx-class                  pic 9(2).
           03  ac2day-pcx-name                   pic x(13).
           03  ac2day-pcx-anterm                 pic 9(3).
           03  ac2day-pcx-antic                  pic +9(06)9.99-.
           03  ac2day-pcx-unearn                 pic +9(06)9.99-.
           03  ac2day-pcx-int                    pic +9(06)9.99-.
           03  ac2day-pcx-addon                  pic +9(06)9.99-.
           03  ac2day-pcx-srv                    pic +9(06)9.99-.
           03  ac2day-pcx-mfee                   pic +9(06)9.99-.
           03  ac2day-pcx-disc                   pic +9(06)9.99-.
           03  ac2day-pcx-other                  pic +9(06)9.99-.
           03  ac2day-pcx-rollback               pic +9(06)9.99-.
           03  ac2day-pcx-contractual            pic 9(04).
           03  ac2day-pcx-actioncd2              pic x(4).
           03  ac2day-pcx-cr                     pic x(1).
           03  ac2day-pcx-lf                     pic x(1).

      ******************************************************************
      *
      *    r e p o r t - f i l e
      *
      *-----------------------------------------------------------------
      *    file used to print a report
      ******************************************************************
       fd  report-file
           label records are standard.

      *=================================================================
       01  report-rec.
           03  report-key.
               05  report-type                       pic x(03).
                   88  report-type-ib                    value "ib ".
                   88  report-type-ibx                   value "ibx".
                   88  report-type-pc                    value "pc ".
                   88  report-type-pcx                   value "pcx".

               05  report-branch                     pic 9(05).
                   88  report-grand-total                value 99999.

               05  report-pool                       pic 9(06).
                   88  report-branch-total               value 999999.

               05  report-class                      pic 9(04).
                   88  report-pool-total                 value 9999.

               05  report-acctno                     pic 9(08).
                   88  report-class-total                value 99999999.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  report-rec-info                       pic x(180).

      *-------------------------------------------------{ int. bearing }
           03  report-ib-rec-info          redefines report-rec-info.
               05  report-ib-display-lnno            pic x(16).
               05  report-ib-lname                   pic x(16).
               05  report-ib-fname                   pic x(16).
               05  report-ib-from-date               pic 9(08).
               05  report-ib-int-since-lstpy         pic s9(9)v99.
               05  report-ib-intbal                  pic s9(9)v99.
               05  report-ib-true-accrue             pic s9(9)v99.
               05  report-ib-accrued                 pic s9(9)v99.
               05  report-ib-max                     pic x(01).
               05  report-ib-number-accts            pic 9(07).
               05  report-ib-actioncd2               pic x(04).
               05  filler                            pic x(68).

      *-------------------------------------{ int. beraring exceptions }
           03  report-ibx-rec-info         redefines report-rec-info.
               05  report-ibx-display-lnno           pic x(16).
               05  report-ibx-lname                  pic x(16).
               05  report-ibx-fname                  pic x(16).
               05  report-ibx-int-pdth-date          pic 9(8).
               05  report-ibx-int-since-lstpy        pic s9(9)v99.
               05  report-ibx-int-owed               pic s9(9)v99.
               05  report-ibx-true-accrue            pic s9(9)v99.
               05  report-ibx-income-accrue          pic s9(9)v99.
               05  report-ibx-balance                pic s9(9)v99.
               05  report-ibx-contr-pdth-date        pic 9(8).
               05  report-ibx-contr-status           pic 9(03).
               05  report-ibx-number-accts           pic 9(07).
               05  report-ibx-actioncd2              pic x(04).
               05  filler                            pic x(47).

      *---------------------------------------------------{ precompute }
           03  report-pc-rec-info          redefines report-rec-info.
               05  report-pc-display-lnno            pic x(16).
               05  report-pc-lname                   pic x(16).
               05  report-pc-fname                   pic x(16).
               05  report-pc-anterm                  pic 9(03).
               05  report-pc-antic                   pic s9(9)v99.
               05  report-pc-unearn                  pic s9(9)v99.
               05  report-pc-int                     pic s9(9)v99.
               05  report-pc-addon                   pic s9(9)v99.
               05  report-pc-srv                     pic s9(9)v99.
               05  report-pc-mfee                    pic s9(9)v99.
               05  report-pc-disc                    pic s9(9)v99.
               05  report-pc-other                   pic s9(9)v99.
               05  report-pc-cash-earn               pic s9(9)v99.
               05  report-pc-ending                  pic s9(9)v99.
               05  report-pc-number-accts            pic 9(07).
               05  report-pc-actioncd2               pic x(04).
               05  filler                            pic x(08).

      *---------------------------------------{precomp exceptions      }
           03  report-pcx-rec-info         redefines report-rec-info.
               05  report-pcx-display-lnno           pic x(16). 
               05  report-pcx-lname                  pic x(16). 
               05  report-pcx-fname                  pic x(16). 
               05  report-pcx-anterm                 pic 9(03).
               05  report-pcx-antic                  pic s9(9)v99.
               05  report-pcx-unearn                 pic s9(9)v99.
               05  report-pcx-int                    pic s9(9)v99.
               05  report-pcx-addon                  pic s9(9)v99.
               05  report-pcx-srv                    pic s9(9)v99.
               05  report-pcx-mfee                   pic s9(9)v99.
               05  report-pcx-disc                   pic s9(9)v99.
               05  report-pcx-other                  pic s9(9)v99.
               05  report-pcx-rollback               pic s9(9)v99.
               05  report-pcx-contractual            pic  9(4).
               05  report-pcx-number-accts           pic 9(07).
               05  report-pcx-actioncd2              pic x(04).
               05  filler                            pic x(08).

      ******************************************************************
      *
      *    w o r k i n g - s t o r a g e - s e c t i o n
      *
      ******************************************************************
       working-storage section.
       77  sccs-identifier pic x(41) value
           "@(#)lp/ac2day.cbl s35 08/09/22-13:12:03 >".

      ******************************************************************
      *
      *    f i l e - w o r k e r s
      *
      ******************************************************************
       copy "libgb/gb01br.cpy".
      *copy "libgb/gb01br_sql.cpy".
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  BR-CURSOR-STAT               PIC X(2)  VALUE SPACES.
           88  BR-CURSOR-STAT-GOOD                VALUE "00".
           88  BR-CURSOR-STAT-BAD                 VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  QBR-WHERE-BEG-KEY.
           03  QBR-WBEG-NO               PIC 9(4).
 
       01  QBR-WHERE-END-KEY.
           03  QBR-WEND-NO               PIC 9(4).
 
       01  QBR-REC.
           03  QBR-NO                                 PIC 9(4).
           03  QBR-NAME                               PIC X(40).
           03  QBR-LOAN-NUM-FLAG                      PIC X.
           03  QBR-FORCE-VALID-CITY-COUNTY            PIC X.
           03  QBR-NOT-OURS-REFUND                    PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
       copy "libgb/gbwsbr.cpy".
       copy "libgb/gb01gr.cpy".
       copy "libgb/gb01gr_sql.cpy".
       copy "libgb/gbwsgr.cpy".

       copy "liblp/lp01bw.cpy".
       copy "liblp/lp01bw_sql.cpy".
       copy "liblp/lpwsbw.cpy".
       copy "liblp/lp01cd.cpy".
       copy "liblp/lp01cd_sql.cpy".
       copy "liblp/lpwscd.cpy".
       copy "liblp/lpwsdl.cpy".
       copy "liblp/lp01lc.cpy".
       copy "liblp/lp01lc_sql.cpy".
       copy "liblp/lpwslc.cpy".
       copy "liblp/lp01ln.cpy".
       copy "liblp/lp01ln_sql.cpy".
       copy "liblp/lpwsln.cpy".
       copy "liblp/lpwslp.cpy".
       copy "liblp/lp01lti.cpy".
       copy "liblp/lp01lti_sql.cpy".
       copy "liblp/lpwslti.cpy".
       copy "liblp/lp01ltp.cpy".
       copy "liblp/lp01ltp_sql.cpy".
       copy "liblp/lpwsltp.cpy".
       copy "liblp/lp01sp.cpy".
       copy "liblp/lp01spa.cpy".
       copy "liblp/lp01spb.cpy".
       copy "liblp/lp01spc.cpy".
       copy "liblp/lp01sp_sql.cpy".
       copy "liblp/lpwssp.cpy".

      *-----------------------------------------------------------------
       01  report-ifn                  pic x(06)     value "report".
       01  report-path                 pic x(35)     value spaces.

      *=================================================================
      *01  local-file-paths.
      *    03  local-bw-path              pic x(35).
      *    03  local-dl-path              pic x(35).
      *    03  local-lp-path              pic x(35).

      *=================================================================
       01  ac2day-ib-key                pic 9(09)     value zeroes.
       01  ac2day-ibx-key               pic 9(09)     value zeroes.
       01  ac2day-pc-key                pic 9(09)     value zeroes.
       01  ac2day-pcx-key               pic 9(09)     value zeroes.

       01  ac2day-ib-path               pic x(35)     value spaces.
       01  filler                       redefines ac2day-ib-path.
           03  ac2day-ib-path-slash     pic x(01).
           03  ac2day-ib-path-fil       pic x(12).
           03  ac2day-ib-path-ftp       pic x(04).
           03  ac2day-ib-path-filename  pic x(18).

       01  ac2day-ibx-path              pic x(35)     value spaces.
       01  filler                       redefines ac2day-ibx-path.
           03  ac2day-ibx-path-slash    pic x(01).
           03  ac2day-ibx-path-fil      pic x(12).
           03  ac2day-ibx-path-ftp      pic x(04).
           03  ac2day-ibx-path-filename pic x(18).

       01  ac2day-pc-path               pic x(35)     value spaces.
       01  filler                       redefines ac2day-pc-path.
           03  ac2day-pc-path-slash     pic x(01).
           03  ac2day-pc-path-fil       pic x(12).
           03  ac2day-pc-path-ftp       pic x(04).
           03  ac2day-pc-path-filename  pic x(18).

       01  ac2day-pcx-path              pic x(35)     value spaces.
       01  filler                       redefines ac2day-pcx-path.
           03  ac2day-pcx-path-slash    pic x(01).
           03  ac2day-pcx-path-fil      pic x(12).
           03  ac2day-pcx-path-ftp      pic x(04).
           03  ac2day-pcx-path-filename pic x(18).

      ******************************************************************
      *
      *    l o a d - w o r k e r s
      *
      *-----------------------------------------------------------------
      *    workers used to load the report-file
      ******************************************************************

      *-------------------------------------------------{ int. bearing }
       01  load-report-ib-workers.
           03  load-ib-display-lnno                  pic x(16).
           03  load-ib-lname                         pic x(16).
           03  load-ib-fname                         pic x(16).
           03  load-ib-from-date                     pic 9(08).
           03  load-ib-int-since-lstpy               pic s9(9)v99.
           03  load-ib-intbal                        pic s9(9)v99.
           03  load-ib-true-accrue                   pic s9(9)v99.
           03  load-ib-accrued                       pic s9(9)v99.
           03  load-ib-max                           pic x(01).
           03  load-ib-actioncd2                     pic x(04).

      *-------------------------------------{ int. beraring exceptions }
       01  load-report-ibx-workers.
           03  load-ibx-display-lnno                 pic x(16).
           03  load-ibx-lname                        pic x(16).
           03  load-ibx-fname                        pic x(16).
           03  load-ibx-int-pdth-date                pic 9(8).
           03  load-ibx-int-since-lstpy              pic s9(9)v99.
           03  load-ibx-int-owed                     pic s9(9)v99.
           03  load-ibx-true-accrue                  pic s9(9)v99.
           03  load-ibx-income-accrue                pic s9(9)v99.
           03  load-ibx-balance                      pic s9(9)v99.
           03  load-ibx-contr-pdth-date              pic 9(8).
           03  load-ibx-contr-status                 pic 9(03).
           03  load-ibx-actioncd2                    pic x(04).

      *---------------------------------------------------{ precompute }
       01  load-report-pc-workers.
           03  load-pc-display-lnno                  pic x(16).
           03  load-pc-lname                         pic x(16).
           03  load-pc-fname                         pic x(16).
           03  load-pc-anterm                        pic 9(03).
           03  load-pc-antic                         pic s9(9)v99.
           03  load-pc-unearn                        pic s9(9)v99.
           03  load-pc-int                           pic s9(9)v99.
           03  load-pc-addon                         pic s9(9)v99.
           03  load-pc-srv                           pic s9(9)v99.
           03  load-pc-mfee                          pic s9(9)v99.
           03  load-pc-disc                          pic s9(9)v99.
           03  load-pc-other                         pic s9(9)v99.
           03  load-pc-cash-earn                     pic s9(9)v99.
           03  load-pc-ending                        pic s9(9)v99.
           03  load-pc-max                           pic x.
           03  load-pc-actioncd2                     pic x(04).

       01  load-report-pcx-workers.
           03  load-pcx-display-lnno                 pic x(16). 
           03  load-pcx-lname                        pic x(16). 
           03  load-pcx-fname                        pic x(16). 
           03  load-pcx-anterm                       pic 9(03).
           03  load-pcx-antic                        pic s9(9)v99.
           03  load-pcx-unearn                       pic s9(9)v99.
           03  load-pcx-int                          pic s9(9)v99.
           03  load-pcx-addon                        pic s9(9)v99.
           03  load-pcx-srv                          pic s9(9)v99.
           03  load-pcx-mfee                         pic s9(9)v99.
           03  load-pcx-disc                         pic s9(9)v99.
           03  load-pcx-other                        pic s9(9)v99.
           03  load-pcx-rollback                     pic s9(9)v99.
           03  load-pcx-contractual                  pic  9(4).
           03  load-pcx-actioncd2                    pic x(04).
 
      ******************************************************************
      *
      *    m i s c e l l a n e o u s - w o r k e r s
      *
      ******************************************************************
       01  calcualtion-workers.
           03  calc-addon-earnings     pic s9(7)v99  comp value zeroes.
           03  calc-intpdth-date       pic 9(8)     value zeroes.


       01  current-report-type         pic x(03)     value spaces.
           88  current-report-type-ib                value "ib ".
           88  current-report-type-ibx               value "ibx".
           88  current-report-type-pc                value "pc ".
           88  current-report-type-pcx               value "pcx".

       01  wss-lps-display-lnno        value spaces.
           03  wss-lps-class           pic 99.
           03  wss-lps-dash            pic x.
           03  wss-lps-number          pic 9(6).
           03  wss-lps-7status         pic x(7).
       01  filler                      redefines wss-lps-display-lnno.
           03  filler                  pic x(3).
           03  wss-lps-number-7status  pic x(13).

       01  hold-branch                 pic 9(05)     value zeroes.
       01  hold-class                  pic 9(03)     value zeroes.
       01  hold-pool                   pic 9(06)     value zeroes.

       01  hold-lc1-key.
           03  hold-lc-code            pic 9(3)      value zeroes.

       01  hold-report-key.
           03  hold-report-type        pic x(03).
               88  hold-report-type-ib               value "ib ".
               88  hold-report-type-ibx              value "ibx".
               88  hold-report-type-pc               value "pc ".
               88  hold-report-type-pcx              value "pcx".

           03  hold-report-branch      pic 9(05).
               88  hold-report-grand-total           value 99999.
           03  hold-report-pool        pic 9(06).
               88  hold-report-branch-total          value 999999.
           03  hold-report-class       pic 9(04).
               88  hold-report-pool-total            value 9999.
           03  hold-report-acctno      pic 9(08).
               88  hold-report-class-total           value 99999999.

       01  legend                      pic x(79)     value spaces.
       01  legend-list                 pic x(09)     value "legend:b.".
       01  max-ib-interest             pic s9(9)v99  comp  value zeroes.
       01  one                         pic s9(1)     value zeroes.

       01  process-acctno-stat         pic x(02)     value spaces.
           88  process-acctno-stat-continue          value spaces.
           88  process-acctno-stat-completed         value "xx".

       01  ln-count                pic 9(03)     value 99.
       01  ln-feed                 pic 9(02)     value zeroes.
       01  page-number-total       pic 9(04)     value zeroes.
       01  page-number-type        pic 9(04)     value zeroes.

       01  records-found               pic x(01)     value "x".
           88  no-records-found                      value "n".
           88  exit-prior-to-execution               value "x".

       01  starting-time               pic x(08)     value spaces.
       01  sub-report-type             pic 9(4)      value zeros.

      ******************************************************************
      *
      *    s c r e e n - w o r k e r s
      *
      ******************************************************************
       copy "libgb/eobuf_ext.cpy".
      *-----------------------------------------------------------------
           03  beg-branch                         pic 9(04).
           03  end-branch                         pic 9(04).

           03  accrual-date                       pic 9(08).
           03  filler                   redefines accrual-date.
               05  accrual-date-cc                pic 9(02).
               05  accrual-date-yy                pic 9(02).
               05  accrual-date-mm                pic 9(02).
               05  accrual-date-dd                pic 9(02).

           03  group-flag                     pic x(01).
           03  ent-group                      pic x(04).
           03  local-print-fg                 pic x(01).

           03  filler                             pic x(01).

           03  beg-pool                       pic 9(05).
           03  end-pool                       pic 9(05).

           03  report-breakdown-options.
               05  print-by-class                 pic x(01).
               05  print-by-pool                  pic x(01).

           03  report-loan-type-options.
               05  print-loan-type-ib             pic x(01).
               05  print-loan-type-ibx            pic x(01).
               05  print-loan-type-pc             pic x(01).
               05  print-loan-type-pcx            pic x(01).

           03  report-print-options.
               05  print-branch-totals            pic x(01).
               05  print-class-totals             pic x(01).
               05  print-detail-analysis          pic x(01).
               05  print-grand-totals             pic x(01).
               05  print-pool-totals              pic x(01).

           03  export-file-yno                pic x(01).

      *-----------------------------------------------------------------
       01  version-control             pic x(01)     value "e".

       01  save-screen-ranges.
           03  sv-beg-branch           pic 9(04)     value 9999.
           03  sv-end-branch           pic 9(04)     value 9999.

           03  sv-group-option.
               05  sv-group-flag       pic x(01).
               05  sv-ent-group        pic x(04).
               05  sv-local-print-fg   pic x(01).

      ******************************************************************
      *
      *    h e a d e r - w o r k e r s
      *
      ******************************************************************
       01  header-line-01.
           03  h01-date           pic x(08) value spaces.
           03  filler             pic x(02) value spaces.
           03  h01-hour           pic 9(02) value zeroes.
           03  filler             pic x(01) value ":".
           03  h01-minute         pic 9(02) value zeroes.
           03  filler             pic x(30) value spaces.
           03  h01-name           pic x(40) value spaces.
           03  filler             pic x(20) value spaces.
           03  filler             pic x(18) value "page (type/total) ".
           03  h01-page-type      pic 9(04).
           03  filler             pic x(01) value "/".
           03  h01-page-total     pic 9(04).

      *-----------------------------------------------------------------
       01  header-line-02.
           03  filler             pic x(10) value "user id:".
           03  h02-userid         pic x(10) value spaces.
           03  filler             pic x(18) value spaces.
           03  h02-title          pic x(92) value spaces.
           03  filler             pic x(02) value spaces.
           03  filler             pic x(12) value "day ending: ".
           03  h02-accrual-date   pic 99/99/99.

      *-----------------------------------------------------------------
       01  header-line-03.
           03  filler             pic x(08) value "branch :".
           03  filler             pic x(02) value spaces.
           03  h03-branch         pic 9(04) value zeroes.
           03  h03-branchx        redefines h03-branch
                                  pic x(04).
           03  filler             pic x(02) value spaces.
           03  h03-branch-desc    pic x(40) value spaces.
           03  filler             pic x(76) value spaces.

      *-----------------------------------------------------------------
       01  header-line-04.
           03  filler             pic x(08) value "pool id:".
           03  filler             pic x(02) value spaces.
           03  h04-pool           pic 9(05) value zeroes.
           03  h04-poolx          redefines h04-pool
                                  pic x(05).
           03  filler             pic x(01) value spaces.
           03  h04-pool-desc      pic x(40) value spaces.
           03  filler             pic x(76) value spaces.

      *-----------------------------------------------------------------
       01  header-line-05.
           03  filler             pic x(08) value "class  :".
           03  filler             pic x(02) value spaces.
           03  h05-class          pic 9(02) value zeroes.
           03  h05-classx         redefines h05-class
                                  pic x(02).
           03  filler             pic x(04) value spaces.
           03  h05-class-desc     pic x(40) value spaces.
           03  filler             pic x(56) value spaces.
           03  h05-mtd-non        pic x(07) value "mtd-non".
           03  filler             pic x(11) value spaces.

      *-------------------------------------------{ 'interest bearing' }
       01  header-line-ib-01.
           03  filler pic x(30) value spaces.
           03  filler pic x(30) value "              accrued     accr".
           03  filler pic x(30) value "ued from    +    interest     ".
           03  filler pic x(30) value "=       true        accrued   ".
           03  filler pic x(12) value spaces.

      *-----------------------------------------------------------------
       01  header-line-ib-02.
           03  filler pic x(30) value "account no.   borrower        ".
           03  filler pic x(30) value "                 from    last ".
           03  filler pic x(30) value "assessed             owed     ".
           03  filler pic x(30) value "    assessed         amount   ".
           03  filler pic x(12) value spaces.

      *--------------------------------{ 'interest bearing exceptions' }
       01  header-line-ibx-01.
           03  filler pic x(30) value spaces.
           03  filler pic x(30) value "        acrrued    accrd from ".
           03  filler pic x(30) value "    interest         true     ".
           03  filler pic x(30) value " accrued                contra".
           03  filler pic x(12) value "ct    cont  ".

      *-----------------------------------------------------------------
       01  header-line-ibx-02.
           03  filler pic x(30) value "account no.   borrower        ".
           03  filler pic x(30) value "        from        last assd ".
           03  filler pic x(30) value "        owed         assd     ".
           03  filler pic x(30) value "  amount      balance  paid th".
           03  filler pic x(12) value "ru  status  ".

      *-------------------------------------------------{ 'precompute' }
       01  header-line-pc-01.
           03  filler pic x(30) value "                             a".
           03  filler pic x(30) value "nticipated      begin   ------".
           03  filler pic x(30) value "---------- daily accrual earni".
           03  filler pic x(30) value "ngs ----------------    accrua".
           03  filler pic x(12) value "l    ending ".

      *-----------------------------------------------------------------
       01  header-line-pc-02.
           03  filler pic x(30) value "account no.   borrower      te".
           03  filler pic x(30) value "rm earnings  unearned  interes".
           03  filler pic x(30) value "t    addons    serchg  maintfe".
           03  filler pic x(30) value "e   dl disc     other  earning".
           03  filler pic x(12) value "s  unearned ".

      *-----------------------------------------------{'precomp exception'}
       01  header-line-pcx-01.
           03  filler pic x(30) value "                             a".
           03  filler pic x(30) value "nticipated      begin   ------".
           03  filler pic x(30) value "---------- daily accrual earni".
           03  filler pic x(30) value "ngs ----------------    earnin".
           03  filler pic x(12) value "gs          ".
 
      *-----------------------------------------------{'precomp exception'}
       01  header-line-pcx-02.
           03  filler pic x(30) value "account no.   borrower      te".
           03  filler pic x(30) value "rm earnings  unearned  interes".
           03  filler pic x(30) value "t    addons    serchg  maintfe".
           03  filler pic x(30) value "e   dl disc     other   rollba".
           03  filler pic x(12) value "ck contract.".

      *---------------------------------{ 'interest bearing' (summary) }
       01  header-line-ib-01-summary.
           03  filler pic x(30) value "* summary *                   ".
           03  filler pic x(30) value spaces.
           03  filler pic x(30) value spaces.
           03  filler pic x(30) value "                              ".
           03  filler pic x(12) value spaces.

      *-----------------------------------------------------------------
       01  header-line-ib-02-summary.
           03  filler pic x(30) value "                              ".
           03  filler pic x(30) value "        total accrued         ".
           03  filler pic x(30) value "   total       total true     ".
           03  filler pic x(30) value " total accrued                ".
           03  filler pic x(12) value spaces.

      *-----------------------------------------------------------------
       01  header-line-ib-03-summary.
           03  filler pic x(30) value " br# pool cl        accts     ".
           03  filler pic x(30) value "   from last assessed         ".
           03  filler pic x(30) value "int owed         assessed     ".
           03  filler pic x(30) value "        amount                ".
           03  filler pic x(12) value spaces.

      *-----------------------{ 'interest bearing exception' (summary) }
       01  header-line-ibx-01-summary.
           03  filler pic x(30) value "* summary *                   ".
           03  filler pic x(30) value spaces.
           03  filler pic x(30) value spaces.
           03  filler pic x(30) value "                              ".
           03  filler pic x(12) value spaces.

      *-----------------------------------------------------------------
       01  header-line-ibx-02-summary.
           03  filler pic x(30) value spaces.
           03  filler pic x(30) value "                   accrd from ".
           03  filler pic x(30) value "    interest         true     ".
           03  filler pic x(30) value " accrued                      ".
           03  filler pic x(12) value "            ".

      *-----------------------------------------------------------------
       01  header-line-ibx-03-summary.
           03  filler pic x(30) value " br# pool cl                  ".
           03  filler pic x(30) value "                    last assd ".
           03  filler pic x(30) value "        owed         assd     ".
           03  filler pic x(30) value "  amount      balance         ".
           03  filler pic x(12) value "            ".

      *---------------------------------------{ 'precompute' (summary) }
       01  header-line-pc-01-summary.
           03  filler pic x(30) value "* summary *                   ".
           03  filler pic x(30) value spaces.
           03  filler pic x(30) value spaces.
           03  filler pic x(21) value spaces.
           03  filler pic x(07) value "mtd-non".
           03  filler pic x(14) value spaces.

      *-----------------------------------------------------------------
       01  header-line-pc-02-summary.
           03  filler pic x(30) value "                   no      beg".
           03  filler pic x(30) value "in       ---------------------".
           03  filler pic x(30) value " daily accrual earnings ------".
           03  filler pic x(30) value "---------------       accrual ".
           03  filler pic x(12) value "     ending ".

      *-----------------------------------------------------------------
       01  header-line-pc-03-summary.
           03  filler pic x(30) value " br# pool cl    accts   unearn".
           03  filler pic x(30) value "ed     interest      addons   ".
           03  filler pic x(30) value "   serchg    maintfee      dl ".
           03  filler pic x(30) value "disc       other    earnings  ".
           03  filler pic x(12) value "   unearned ".

    
      *---------------------------------------{ 'precompute' (summary) }
      *                                         'exceptions'
       01  header-line-pcx-01-summary.
           03  filler pic x(30) value "* summary *                   ".
           03  filler pic x(30) value spaces.
           03  filler pic x(30) value spaces.
           03  filler pic x(21) value spaces.
           03  filler pic x(07) value "mtd-non".
           03  filler pic x(14) value spaces.
 
      *-----------------------------------------------------------------
       01  header-line-pcx-02-summary.
           03  filler pic x(30) value "                   no      beg".
           03  filler pic x(30) value "in       ---------------------".
           03  filler pic x(30) value " daily accrual earnings ------".
           03  filler pic x(30) value "---------------       accrual ".
           03  filler pic x(12) value "     ending ".

      *-----------------------------------------------------------------
       01  header-line-pcx-03-summary.
           03  filler pic x(30) value " br# pool cl    accts   unearn".
           03  filler pic x(30) value "ed     interest      addons   ".
           03  filler pic x(30) value "   serchg    maintfee      dl ".
           03  filler pic x(30) value "disc       other    earnings  ".
           03  filler pic x(12) value "   unearned ".

      *-----------------------------------------------------------------
       01  header-title-type-ib.
           03  filler pic x(31) value "accrued interest for i/b loans ".
           03  filler pic x(31) value "  (* - exceeds max accrue days)".
           03  filler pic x(30) value "                              ".

      *-----------------------------------------------------------------
       01  header-title-type-ibx.
           03  filler pic x(31) value "accrued interest for i/b loans ".
           03  filler pic x(31) value "exceeding max accrue days      ".
           03  filler pic x(30) value "                              ".

      *-----------------------------------------------------------------
       01  header-title-type-pc.
           03  filler pic x(31) value "unearned / earned accrual repor".
           03  filler pic x(31) value "t (precompute)                 ".
           03  filler pic x(30) value "                              ".

      *-----------------------------------------------------------------
       01  header-title-type-pcx.
           03  filler pic x(31) value "unearned / exceptions     repor".
           03  filler pic x(31) value "t (precompute)                 ".
           03  filler pic x(30) value "                              ".
 
      ******************************************************************
      *
      *    d e t a i l - l i n e
      *
      ******************************************************************
       01  detail-line                        pic x(132)   value spaces.

      *-----------------------------------------------{ 'int. bearing' }
       01  detail-line-01-ib                  redefines detail-line.
           03  d01-ib-display-lnno            pic x(13).
           03  filler                         pic x(01).
           03  d01-ib-name                    pic x(13).
           03  filler                         pic x(16).
           03  d01-ib-from                    pic 99/99/99.
           03  filler                         pic x(05).
           03  d01-ib-int-since-lstpy         pic z,zzz,zz9.99.
           03  filler                         pic x(05).
           03  d01-ib-intbal                  pic z,zzz,zz9.99.
           03  filler                         pic x(05).
           03  d01-ib-true-accrue             pic z,zzz,zz9.99.
           03  filler                         pic x(05).
           03  d01-ib-accrued                 pic zzz,zz9.99.
           03  filler                         pic x(03).
           03  d01-ib-max                     pic x(01).

      *------------------------------------{ 'int. bearing exceptions' }
       01  detail-line-02-ibx                 redefines detail-line.
           03  d02-ibx-display-lnno           pic x(13).
           03  filler                         pic x(01).
           03  d02-ibx-name                   pic x(13).
           03  filler                         pic x(11).
           03  d02-ibx-int-pdth               pic 99/99/99.
           03  filler                         pic x(01).
           03  d02-ibx-int-since-lstpy        pic z,zzz,zz9.99.
           03  filler                         pic x(01).
           03  d02-ibx-int-owed               pic z,zzz,zz9.99.
           03  filler                         pic x(01).
           03  d02-ibx-true-accrue            pic z,zzz,zz9.99.
           03  filler                         pic x(03).
           03  d02-ibx-income-accrue          pic zzz,zz9.99.
           03  filler                         pic x(01).
           03  d02-ibx-balance                pic z,zzz,zz9.99.
           03  filler                         pic x(03).
           03  d02-ibx-cont-pdth              pic 99/99/99.
           03  filler                         pic x(05).
           03  d02-ibx-cont-status            pic zz9.
           03  filler                         pic x(02).

      *-------------------------------------------------{ 'precompute' }
       01  detail-line-03-pc                  redefines detail-line.
           03  d03-pc-display-lnno            pic x(13).
           03  filler                         pic x(01).
           03  d03-pc-name                    pic x(13).
           03  filler                         pic x(01).
           03  d03-pc-anterm                  pic zz9        blank zero.
           03  filler                         pic x(01).
           03  d03-pc-antic                   pic z(05)9.99- blank zero.
           03  d03-pc-unearn                  pic z(05)9.99- blank zero.
           03  d03-pc-int                     pic z(05)9.99- blank zero.
           03  d03-pc-addon                   pic z(05)9.99- blank zero.
           03  d03-pc-srv                     pic z(05)9.99- blank zero.
           03  d03-pc-mfee                    pic z(05)9.99- blank zero.
           03  d03-pc-disc                    pic z(05)9.99- blank zero.
           03  d03-pc-other                   pic z(05)9.99- blank zero.
           03  d03-pc-cash-earn               pic z(05)9.99- blank zero.
           03  d03-pc-ending                  pic z(05)9.99- blank zero.


      *-------------------------------------------------{ 'precompute' }
      *                                                   'exceptions' 
       01  detail-line-04-pcx                redefines detail-line.
           03  d04-pcx-display-lnno           pic x(13).
           03  filler                         pic x(01).
           03  d04-pcx-name                   pic x(13).
           03  filler                         pic x(01).
           03  d04-pcx-anterm                 pic zz9        blank zero.
           03  filler                         pic x(01).
           03  d04-pcx-antic                  pic z(05)9.99- blank zero.
           03  d04-pcx-unearn                 pic z(05)9.99- blank zero.
           03  d04-pcx-int                    pic z(05)9.99- blank zero.
           03  d04-pcx-addon                  pic z(05)9.99- blank zero.
           03  d04-pcx-srv                    pic z(05)9.99- blank zero.
           03  d04-pcx-mfee                   pic z(05)9.99- blank zero.
           03  d04-pcx-disc                   pic z(05)9.99- blank zero.
           03  d04-pcx-other                  pic z(05)9.99- blank zero.
           03  d04-pcx-rollback               pic z(05)9.99- blank zero.
           03  filler                         pic x(3).
           03  d04-pcx-contractual            pic 9(04)      blank zero.

      *-------------------------------------------------------{ ranges }
       01  range-line-01                      redefines detail-line.
           03  r01-prompt                     pic x(32).
           03  r01-field                      pic x(100).
           03  r01-date                       redefines r01-field
                                              pic 99/99/99.
           03  r01-numeric                    redefines r01-field
                                              pic z(10)9.
           03  r01-text                       redefines r01-field
                                              pic x(100).

      *--------------------------------{ 'int. bearing' summary totals }
       01  summary-line-01-ib                 redefines detail-line.
           03  s01-ib-branch                  pic z(03)9.
           03  s01-ib-branchx                 redefines s01-ib-branch
                                              pic x(04).
           03  filler                         pic x(01).
           03  s01-ib-pool                    pic z(03)9.
           03  s01-ib-poolx                   redefines s01-ib-pool
                                              pic x(04).
           03  filler                         pic x(01).
           03  s01-ib-class                   pic 9(02).
           03  s01-ib-classx                  redefines s01-ib-class
                                              pic x(02).
           03  filler                         pic x(05).
           03  s01-ib-number-accts            pic z(05)9.
           03  s01-ib-number-accts-fg         pic x(01).
           03  filler                         pic x(13).
           03  s01-ib-int-since-lstpy         pic zzz,zzz,zz9.99.
           03  filler                         pic x(03).
           03  s01-ib-intbal                  pic zzz,zzz,zz9.99.
           03  filler                         pic x(03).
           03  s01-ib-true-accrue             pic zzz,zzz,zz9.99.
           03  filler                         pic x(05).
           03  s01-ib-accrued                 pic zzz,zzz,zz9.99.

      *-------------------------{ 'int. bearing execpt' summary totals }
       01  summary-line-02-ibx                redefines detail-line.
           03  s02-ibx-branch                 pic z(03)9.
           03  s02-ibx-branchx                redefines s02-ibx-branch
                                              pic x(04).
           03  filler                         pic x(01).
           03  s02-ibx-pool                   pic z(03)9.
           03  s02-ibx-poolx                  redefines s02-ibx-pool
                                              pic x(04).
           03  filler                         pic x(01).
           03  s02-ibx-class                  pic 9(02).
           03  s02-ibx-classx                 redefines s02-ibx-class
                                              pic x(02).
           03  filler                         pic x(05).
           03  s02-ibx-number-accts           pic z(05)9.
           03  s02-ibx-number-accts-fg        pic x(01).
           03  filler                         pic x(23).
           03  s02-ibx-int-since-lstpy        pic z,zzz,zz9.99.
           03  filler                         pic x(01).
           03  s02-ibx-int-owed               pic z,zzz,zz9.99.
           03  filler                         pic x(1).
           03  s02-ibx-true-accrue            pic z,zzz,zz9.99.
           03  filler                         pic x(1).
           03  s02-ibx-income-accrue          pic z,zzz,zz9.99.
           03  filler                         pic x(1).
           03  s02-ibx-balance                pic z,zzz,zz9.99.

      *----------------------------------{ 'precompute' summary totals }
       01  summary-line-03-pc                 redefines detail-line.
           03  s03-pc-branch                  pic z(03)9.
           03  s03-pc-branchx                 redefines s03-pc-branch
                                              pic x(04).
           03  filler                         pic x(01).
           03  s03-pc-pool                    pic z(03)9.
           03  s03-pc-poolx                   redefines s03-pc-pool
                                              pic x(04).
           03  filler                         pic x(01).
           03  s03-pc-class                   pic 9(02).
           03  s03-pc-classx                  redefines s03-pc-class
                                              pic x(02).
           03  filler                         pic x(01).
           03  s03-pc-number-accts            pic z(05)9.
           03  s03-pc-number-accts-fg         pic x(01).
           03  s03-pc-unearn                  pic z(08)9.99- blank zero.
           03  s03-pc-int                     pic z(08)9.99- blank zero.
           03  s03-pc-addon                   pic z(07)9.99- blank zero.
           03  s03-pc-srv                     pic z(07)9.99- blank zero.
           03  s03-pc-mfee                    pic z(07)9.99- blank zero.
           03  s03-pc-disc                    pic z(08)9.99- blank zero.
           03  s03-pc-other                   pic z(07)9.99- blank zero.
           03  s03-pc-cash-earn               pic z(07)9.99- blank zero.
           03  s03-pc-ending                  pic z(08)9.99- blank zero.

       01  summary-line-04-pcx                 redefines detail-line.
           03  s04-pcx-branch                  pic z(03)9.
           03  s04-pcx-branchx                 redefines s04-pcx-branch
                                              pic x(04).
           03  filler                         pic x(01).
           03  s04-pcx-pool                    pic z(03)9.
           03  s04-pcx-poolx                   redefines s04-pcx-pool
                                              pic x(04).
           03  filler                         pic x(01).
           03  s04-pcx-class                   pic 9(02).
           03  s04-pcx-classx                  redefines s04-pcx-class
                                              pic x(02).
           03  filler                         pic x(01).
           03  s04-pcx-number-accts            pic z(05)9.
           03  s04-pcx-number-accts-fg         pic x(01).
           03  s04-pcx-unearn                 pic z(08)9.99- blank zero.
           03  s04-pcx-int                    pic z(08)9.99- blank zero.
           03  s04-pcx-addon                  pic z(07)9.99- blank zero.
           03  s04-pcx-srv                    pic z(07)9.99- blank zero.
           03  s04-pcx-mfee                   pic z(07)9.99- blank zero.
           03  s04-pcx-disc                   pic z(08)9.99- blank zero.
           03  s04-pcx-other                  pic z(07)9.99- blank zero.
           03  s04-pcx-rollback               pic z(07)9.99- blank zero.


      *-------------------------------{ 'int. bearing' standard totals }
       01  total-line-01-ib                   redefines detail-line.
           03  t01-ib-prompt                  pic x(25).
           03  t01-ib-number-accts            pic z(05)9.
           03  t01-ib-number-accts-fg         pic x(01).
           03  filler                         pic x(23).
           03  t01-ib-int-since-lstpy         pic zz,zzz,zz9.99.
           03  filler                         pic x(04).
           03  t01-ib-intbal                  pic zz,zzz,zz9.99.
           03  filler                         pic x(04).
           03  t01-ib-true-accrue             pic zz,zzz,zz9.99.
           03  filler                         pic x(01).
           03  t01-ib-accrued                 pic zzz,zzz,zz9.99-.

      *------------------------{ 'int. bearing execpt' standard totals }
       01  total-line-02-ibx                  redefines detail-line.
           03  t02-ibx-prompt                 pic x(25).
           03  t02-ibx-number-accts           pic z(05)9.
           03  t02-ibx-number-accts-fg        pic x(01).
           03  filler                         pic x(15).
           03  t02-ibx-int-since-lstpy        pic z,zzz,zz9.99.
           03  filler                         pic x(01).
           03  t02-ibx-int-owed               pic z,zzz,zz9.99.
           03  filler                         pic x(1).
           03  t02-ibx-true-accrue            pic z,zzz,zz9.99.
           03  filler                         pic x(1).
           03  t02-ibx-income-accrue          pic z,zzz,zz9.99.
           03  filler                         pic x(1).
           03  t02-ibx-balance                pic z,zzz,zz9.99.

      *---------------------------------{ 'precompute' standard totals }
       01  total-line-03-pc                   redefines detail-line.
           03  t03-pc-prompt                  pic x(25).
           03  t03-pc-number-accts            pic z(05)9.
           03  t03-pc-number-accts-fg         pic x(01).
           03  filler                         pic x(07).
           03  t03-pc-unearn                  pic z(08)9.99- blank zero.
           03  filler                         pic x(07).
           03  t03-pc-addon                   pic z(08)9.99- blank zero.
           03  filler                         pic x(07).
           03  t03-pc-mfee                    pic z(08)9.99- blank zero.
           03  filler                         pic x(07).
           03  t03-pc-other                   pic z(08)9.99- blank zero.
           03  filler                         pic x(07).
           03  t03-pc-ending                  pic z(08)9.99- blank zero.

      *-----------------------------------------------------------------
       01  total-line-04-pc                   redefines detail-line.
           03  filler                         pic x(49).
           03  t04-pc-int                     pic z(08)9.99- blank zero.
           03  filler                         pic x(07).
           03  t04-pc-srv                     pic z(08)9.99- blank zero.
           03  filler                         pic x(07).
           03  t04-pc-disc                    pic z(08)9.99- blank zero.
           03  filler                         pic x(07).
           03  t04-pc-cash-earn               pic z(08)9.99- blank zero.


      *---------------------------------{ 'precompute' exceptions      }
       01  total-line-03-pcx                  redefines detail-line.
           03  t03-pcx-prompt                 pic x(25).
           03  t03-pcx-number-accts           pic z(05)9.
           03  t03-pcx-number-accts-fg        pic x(01).
           03  filler                         pic x(07).
           03  t03-pcx-unearn                 pic z(08)9.99- blank zero.
           03  filler                         pic x(07).
           03  t03-pcx-addon                  pic z(08)9.99- blank zero.
           03  filler                         pic x(07).
           03  t03-pcx-mfee                   pic z(08)9.99- blank zero.
           03  filler                         pic x(07).
           03  t03-pcx-other                  pic z(08)9.99- blank zero.
           03  filler                         pic x(07).

      *-----------------------------------------------------------------
       01  total-line-04-pcx                  redefines detail-line.
           03  filler                         pic x(49).
           03  t04-pcx-int                    pic z(08)9.99- blank zero.
           03  filler                         pic x(07).
           03  t04-pcx-srv                    pic z(08)9.99- blank zero.
           03  filler                         pic x(07).
           03  t04-pcx-disc                   pic z(08)9.99- blank zero.
           03  filler                         pic x(07).
           03  t04-pcx-rollback               pic z(08)9.99- blank zero.


      ******************************************************************
      *
      *    c o p y - l i b r a r y - w o r k e r s
      *
      ******************************************************************
cc32  *copy "libcl/rvcalcw.cpy".     *> cc32

       copy "libgb/daccessw.cpy".
       copy "libgb/accessw.cpy".
       copy "libgb/ageingw.cpy".
       copy "libgb/brsecurew.cpy".
       copy "libgb/daterw.cpy".
       copy "libgb/eospoolw.cpy".
       copy "libgb/getprw.cpy".
       copy "libgb/gpenvw.cpy".
       copy "libgb/grworkw.cpy".
       copy "libgb/mktempw.cpy".
       copy "libgb/progidw.cpy".
       copy "libgb/rbactw.cpy".
       copy "libgb/rebatew.cpy".
       copy "libgb/passwdw.cpy".
       01  export-file-seq             pic 9(2)      value 01.

       copy "libgb/sppdthw.cpy".
       copy "libgb/timewk.cpy".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       copy "libgb/connectw_sql.cpy".
       copy "libgb/filewk.cpy".
       copy "libgb/messwk.cpy".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       copy "libgb/systemw.cpy".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *copy "liblp/ac1day.cmd.cpy". %%%%%%%%%%%%%%%%% replace this when done
       copy "liblp/ac9day_cmd.cpy".
       copy "liblp/lpaernw.cpy".
       copy "liblp/lpamtsw.cpy".
       copy "liblp/lpaprsw.cpy".
       copy "liblp/lpaprzw.cpy".
       copy "liblp/lpceppw.cpy".
       copy "liblp/lpcernw.cpy".
       copy "liblp/lpeadjw.cpy".
       copy "liblp/lpearnw.cpy".
       copy "liblp/lpibpcw.cpy".
       copy "liblp/lpitrmw.cpy".
       copy "liblp/lpmdtew.cpy".
       copy "liblp/lppduew.cpy".
       copy "liblp/lpratew.cpy".
       copy "liblp/lpregzw.cpy".
       copy "liblp/lpstatw.cpy".
       copy "liblp/lpuernw.cpy".
       copy "liblp/lpuperw.cpy".
       copy "liblp/lpupwkw.cpy".
       copy "liblp/lpxdtew.cpy".
       copy "liblp/setctblw.cpy".

       copy "libwi/pfswindowsw.cpy".
       copy "libwi/tstintw.cpy".
       copy "libwi/grscanw.cpy".

       copy "libgb/getenvw.cpy".
       copy "libgb/coname_ext.cpy".
       copy "libgb/screenw.cpy".
       copy "liblp/ac2day_def.cpy".
       copy "liblp/ac2day_wks.cpy".
       copy "libgb/helplink.cpy".
       copy "libgb/pru_lnk.cpy".

       copy "libgb/getfmw.cpy".
       screen section.
       copy "liblp/ac2day_scn.cpy".

      ******************************************************************
      *
      *    p r o c e d u r e - d i v i s i o n
      *
      ******************************************************************
       procedure division
            using form-pathname exit-pathname.
       declaratives.
       declare-error section.
           use after error procedure on
                 ac2day-ib-file ac2day-ibx-file
                 ac2day-pc-file ac2day-pcx-file
                 report-file pr-file.

       copy "libgb/declrp.cpy".
           perform close-sp1-file.
           perform close-ltp1-file.
           perform close-lti1-file.
           perform close-ln1-file.
           perform close-lc1-file.
           perform close-cd1-file.
           perform close-bw1-file.
           perform close-gr1-file.
           perform close-br-file.
           perform close-pr-file.
           close 
                 ac2day-ib-file ac2day-ibx-file
                 ac2day-pc-file ac2day-pcx-file
                 report-file.
       copy "libgb/declrp2.cpy".

      ******************************************************************
      *
      *    m a i n - p r o g r a m
      *
      ******************************************************************
       main-program section.

           perform sql-connect.

      d    stop mess.
           perform initialization.

           if ( ext-eobuf-errcd not = spaces )
              go to main-program-exit.

           perform execution-module.

       main-program-exit.
           go to end-program.

      ******************************************************************
       initialization section.
      ******************************************************************

           move "x"           to records-found.
           move form-pathname to form-path.
           move "ac2day"      to form-prog vdu-form-name.

           move zeroes to accrual-date.

           move ac2day-queue-pos  to q-pos.
           move ac2day-copies-pos to c-pos.
           move ac2day-pru-pos    to p-pos.
           move spaces            to q-buf c-buf p-buf.

       copy "libgb/eoinit.cpy".

           if ext-route-buf = "00"
              move progid-text-processing to legend
              perform send-legend.

           if ( ext-route-buf = "00" )
              move "s  a010accept." to vdu
              move spaces           to vdubuf
              perform screenio.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           if ( beg-branch = 0 and end-branch = 0 )
              move beg-branch to sv-beg-branch
              move end-branch to sv-end-branch
              move ext-coname-user-loc  to beg-branch
                                           end-branch.

           if ( ext-eobuf-group-fg = "y" )
              move beg-branch         to sv-beg-branch
              move end-branch         to sv-end-branch
              move group-flag         to sv-group-flag
              move ent-group          to sv-ent-group
              move ext-eobuf-group        to ent-group
              move zeroes             to beg-branch
              move 9999               to end-branch
              move "y"                to group-flag
              move ext-eobuf-group-prpath to pru-file.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ make paths }
           if ( export-file-yno = "n" )
              go to initialization-skip-export.

           move "fil" to getenv-buf.
           perform getenv-call.

           if ( stat-bad )
              move "error: environment 'fil' not found" to mess
              perform send-mess
              move "x" to ext-eobuf-errcd
              go to initialization-exit.

           move spaces           to ac2day-ib-path
                                    ac2day-ibx-path
                                    ac2day-pc-path
                                    ac2day-pcx-path.

           move "/"              to ac2day-ib-path-slash
                                    ac2day-ibx-path-slash
                                    ac2day-pc-path-slash
                                    ac2day-pcx-path-slash.

           move getenv-buf       to ac2day-ib-path-fil
                                    ac2day-ibx-path-fil
                                    ac2day-pc-path-fil
                                    ac2day-pcx-path-fil.

           move "ftp/"           to mess.
           call "c$tolower" using mess, value 4.
           move mess             to ac2day-ib-path-ftp
                                    ac2day-ibx-path-ftp
                                    ac2day-pc-path-ftp
                                    ac2day-pcx-path-ftp.

           move "ac2day_ib.txt"  to ac2day-ib-path-filename.
           move "ac2day_ibx.txt" to ac2day-ibx-path-filename.
           move "ac2day_pc.txt"  to ac2day-pc-path-filename.
           move "ac2day_pcx.txt" to ac2day-pcx-path-filename.

           move zeroes           to ac2day-ib-key
                                    ac2day-ibx-key
                                    ac2day-pc-key
                                    ac2day-pcx-key.

           perform open-ac2day-ib-file-output.
           perform open-ac2day-ibx-file-output.
           perform open-ac2day-pc-file-output.
           perform open-ac2day-pcx-file-output.

      *-----------------------------------------------------------------
       initialization-skip-export.

           move mktemp-default to mktemp-buf.
           perform mktemp-call.
           move mktemp-buf     to report-path.
           perform open-report-file-output.
           perform close-report-file.
           perform open-report-file.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           perform get-gpenv.
           perform open-br-file.
           perform open-cd1-file.
           perform open-gr1-file.
           perform open-lc1-file.
           perform open-pr-file.
           perform open-sp1-file.

           perform enable-interrupt.

           perform initial-progress-indicator.
           perform get-time.
           move "n"       to records-found.
           move time-edit to starting-time.
           move "y"       to tstint-continue-only.

       initialization-exit.
           exit.

      ******************************************************************
       enter-parameters section.
      ******************************************************************

           if ( up-key )
              if ( export-file-yno = "n" )
                 go to enter-print-grand-totals
              else
                 go to enter-export-file-yno.

       enter-beg-branch.

           move legend-beg-branch to legend.
           perform send-legend.

           move "e  a000beg-branch." to vdu.
           perform screenio.

           if (        f1-key ) go to enter-parameters-exit.
           if (        f3-key ) go to enter-all-branch.
           if (        f6-key ) perform scan-gr1-file.
           if (        f7-key ) go to enter-parameters-exit.
           if ( not enter-key ) go to enter-beg-branch.

           move vdubuf to wgr-word ent-group.

           if ( wgr-word-zero )
              move zeroes to wgr-sresult
              go to branch-is-numeric.

           move zeroes to wgr-min-level.
           perform check-member.

           if ( wgr-err > zeroes )
              move wgr-msg(wgr-err) to mess
              perform send-mess
              go to enter-beg-branch.

           if ( wgr-type = "a" )
              move "s  a000end-branch." to vdu
              move spaces               to vdubuf
              perform screenio
              move "y"                  to group-flag
              move zeroes               to beg-branch
              move 9999                 to end-branch
              go to check-br-security.

       branch-is-numeric.

           move "n"           to group-flag.
           move wgr-sresult   to beg-branch.

           move "sn n040beg-branch." to vdu.
           move beg-branch           to vdunum0.
           perform screenio.

       enter-end-branch.

           move legend-end-branch to legend.
           perform send-legend.

           move "ennn040end-branch." to vdu.
           perform screenio.

           if (        up-key ) go to enter-beg-branch.
           if (        f1-key ) go to enter-parameters-exit.
           if (        f7-key ) go to enter-parameters-exit.
           if ( not enter-key ) go to enter-end-branch.

           move vdunum0 to end-branch.

           if ( beg-branch > end-branch )
              move "invalid branch range" to mess
              perform send-mess
              go to enter-beg-branch.

           if (( beg-branch = zeroes ) and ( end-branch = zeroes ))
              move "s  a160display-user." to vdu
              move "user branch only"     to vdubuf
              perform screenio
           else
              move "s  a160display-user." to vdu
              move spaces                 to vdubuf
              perform screenio.

           go to check-br-security.

       enter-all-branch.

           move "snnn040beg-branch." to vdu.
           move zeroes               to beg-branch vdunum0.
           perform screenio.

           move "snnn040end-branch." to vdu.
           move 9999                 to end-branch vdunum0.
           perform screenio.

           move "n"                    to group-flag.
           move "s  a160display-user." to vdu.
           move spaces                 to vdubuf.
           perform screenio.

       check-br-security.

           perform br-range-security.

           if ( br-security-okay = "n" ) go to enter-beg-branch.

       enter-beg-pool.

           move legend-beg-pool to legend.
           perform send-legend.

           move "ennn050beg-pool." to vdu.
           perform screenio.

           if (        up-key ) go to enter-beg-branch.
           if (        f1-key ) go to enter-parameters-exit.
           if (        f3-key ) go to enter-all-pool.
           if (        f7-key ) go to enter-parameters-exit.
           if ( not enter-key ) go to enter-beg-pool.

           move vdunum0 to beg-pool.

       enter-end-pool.

           move legend-end-pool to legend.
           perform send-legend.

           move "ennn050end-pool." to vdu.
           perform screenio.

           if (        up-key ) go to enter-beg-pool.
           if (        f1-key ) go to enter-parameters-exit.
           if (        f7-key ) go to enter-parameters-exit.
           if ( not enter-key ) go to enter-end-pool.

           move vdunum0 to end-pool.

           if ( beg-pool > end-pool )
              move "invalid range" to mess
              perform send-mess
              go to enter-beg-pool.

           go to enter-accrual-date.

       enter-all-pool.

           move "snnn050beg-pool." to vdu.
           move zeroes             to vdunum0 beg-pool.
           perform screenio.

           move "snnn050end-pool." to vdu.
           move 99999              to vdunum0 end-pool.
           perform screenio.

       enter-accrual-date.
           if ( ext-route-buf not = "00" )
              if ( up-key )
                 go to enter-end-pool
              else
                 go to enter-export-file-yno.

           perform display-accrual-date.

           move legend-accrual-date to legend.
           perform send-legend.

           move "ernm060accrual-date." to vdu.
           perform screenio.

           if (        up-key ) go to enter-end-pool.
           if (        f1-key ) go to enter-parameters-exit.
           if (        f7-key ) go to enter-parameters-exit.
           if not ( enter-key ) go to enter-accrual-date.

           move vdu-date-yyyymmdd to accrual-date.

       enter-export-file-yno.

           move legend-export-file-yno to legend.
           perform send-legend.

           move "e  a010export-yno." to vdu.
           perform screenio.

           if (        up-key ) go to enter-accrual-date.
           if (        f1-key ) go to enter-parameters-exit.
           if (        f7-key ) go to enter-parameters-exit.
           if ( not enter-key ) go to enter-export-file-yno.

           if ( vdubuf = "y" or "n" or "o" )
              move vdubuf to export-file-yno
           else
              move "error: (y)es, (n)o or (o)nly" to mess
              perform send-mess
              go to enter-export-file-yno.

           if ( export-file-yno not = "n" )
              move export-file-seq               to passwdw-seq
              move "for creating a export file:" to passwdw-prompt2
              perform verify-password
              if passwdw-stat not = "ok"
                 go to enter-export-file-yno.

           if ( export-file-yno not = "o" )
              go to enter-print-by-class.

           move "n" to print-by-class
                       print-by-pool
                       print-loan-type-ib
                       print-loan-type-ibx
                       print-loan-type-pc
                       print-loan-type-pcx
                       print-branch-totals
                       print-class-totals
                       print-detail-analysis
                       print-grand-totals
                       print-pool-totals.

           perform display-parameters.
           move "n" to local-print-fg
           go to enter-accept-yn.

       enter-print-by-class.

           if ( up-key ) go to enter-export-file-yno.

           move "s  a010prt-class." to vdu.
           move "y"                 to vdubuf   print-by-class.
           perform screenio.

       enter-print-by-pool.

           move legend-print-by-pool to legend.
           perform send-legend.

           move "e  a010prt-pool." to vdu.
           perform screenio.

           if (        up-key ) go to enter-print-by-class.
           if (        f1-key ) go to enter-parameters-exit.
           if (        f7-key ) go to enter-parameters-exit.
           if ( not enter-key ) go to enter-print-by-pool.

           move vdubuf to print-by-pool.

           if not ( print-by-pool = "y" or "n" )
              move "enter (y)es or (n)o only!!!" to mess
              perform send-mess
              go to enter-print-by-pool.

       enter-print-loan-type-ib.

           move legend-print-loan-type-ib to legend.
           perform send-legend.

           move "e  a010prt-ib." to vdu.
           perform screenio.

           if (        up-key ) go to enter-print-by-pool.
           if (        f1-key ) go to enter-parameters-exit.
           if (        f7-key ) go to enter-parameters-exit.
           if ( not enter-key ) go to enter-print-loan-type-ib.

           move vdubuf to print-loan-type-ib.

           if not ( print-loan-type-ib = "y" or "n" )
              move "enter (y)es or (n)o only!!!" to mess
              perform send-mess
              go to enter-print-loan-type-ib.

       enter-print-loan-type-ibx.

           move legend-print-loan-type-ibx to legend.
           perform send-legend.

           move "e  a010prt-ibx." to vdu.
           perform screenio.

           if (        up-key ) go to enter-print-loan-type-ib.
           if (        f1-key ) go to enter-parameters-exit.
           if (        f7-key ) go to enter-parameters-exit.
           if ( not enter-key ) go to enter-print-loan-type-ibx.

           move vdubuf to print-loan-type-ibx.

           if not ( print-loan-type-ibx = "y" or "n" )
              move "enter (y)es or (n)o only!!!" to mess
              perform send-mess
              go to enter-print-loan-type-ibx.

       enter-print-loan-type-pc.

           move legend-print-loan-type-pc to legend.
           perform send-legend.

           move "e  a010prt-pc." to vdu.
           perform screenio.

           if (        up-key ) go to enter-print-loan-type-ibx.
           if (        f1-key ) go to enter-parameters-exit.
           if (        f7-key ) go to enter-parameters-exit.
           if ( not enter-key ) go to enter-print-loan-type-pc.

           move vdubuf to print-loan-type-pc.

           if not ( print-loan-type-pc = "y" or "n" )
              move "enter (y)es or (n)o only!!!" to mess
              perform send-mess
              go to enter-print-loan-type-pc.

       enter-print-loan-type-pcx.
           move legend-print-loan-type-pcx to legend.
           perform send-legend.

           move "e  a010prt-pcx." to vdu.
           perform screenio.
   
           if (        up-key ) go to enter-print-loan-type-pc.
           if (        f1-key ) go to enter-parameters-exit.
           if (        f7-key ) go to enter-parameters-exit.
           if ( not enter-key ) go to enter-print-loan-type-pcx.

           move vdubuf to print-loan-type-pcx.

           if not ( print-loan-type-pcx = "y" or "n" )
              move "enter (y)es or (n)o only!!!" to mess
              perform send-mess
              go to enter-print-loan-type-pcx.

       enter-print-detail-analysi.

           move legend-print-detail-analysis to legend.
           perform send-legend.

           move "e  a010prt-detail." to vdu.
           perform screenio.

           if (        up-key ) go to enter-print-loan-type-pc.
           if (        f1-key ) go to enter-parameters-exit.
           if (        f7-key ) go to enter-parameters-exit.
           if ( not enter-key ) go to enter-print-detail-analysi.

           move vdubuf to print-detail-analysis.

           if not ( print-detail-analysis = "y" or "n" )
              move "enter (y)es or (n)o only!!!" to mess
              perform send-mess
              go to enter-print-detail-analysi.

       enter-print-class-totals.

           move legend-print-class-totals to legend.
           perform send-legend.

           move "e  a010prt-classt." to vdu.
           perform screenio.

           if (        up-key ) go to enter-print-detail-analysi.
           if (        f1-key ) go to enter-parameters-exit.
           if (        f7-key ) go to enter-parameters-exit.
           if ( not enter-key ) go to enter-print-class-totals.

           move vdubuf to print-class-totals.

           if not ( print-class-totals = "y" or "n" )
              move "enter (y)es or (n)o only!!!" to mess
              perform send-mess
              go to enter-print-class-totals.

       enter-print-pool-totals.

           if ( print-by-pool = "n" )
              if ( up-key )
                 go to enter-print-class-totals
              else
                 move "s  a010prt-poolt." to vdu
                 move "n"                 to vdubuf   print-pool-totals
                 perform screenio
                 go to enter-print-branch-totals.

           move legend-print-pool-totals to legend.
           perform send-legend.

           move "e  a010prt-poolt." to vdu.
           perform screenio.

           if (        up-key ) go to enter-print-class-totals.
           if (        f1-key ) go to enter-parameters-exit.
           if (        f7-key ) go to enter-parameters-exit.
           if ( not enter-key ) go to enter-print-pool-totals.

           move vdubuf to print-pool-totals.

           if not ( print-pool-totals = "y" or "n" )
              move "enter (y)es or (n)o only!!!" to mess
              perform send-mess
              go to enter-print-pool-totals.

       enter-print-branch-totals.

           move legend-print-branch-totals to legend.
           perform send-legend.

           move "e  a010prt-brancht." to vdu.
           perform screenio.

           if (        up-key ) go to enter-print-pool-totals.
           if (        f1-key ) go to enter-parameters-exit.
           if (        f7-key ) go to enter-parameters-exit.
           if ( not enter-key ) go to enter-print-branch-totals.

           move vdubuf to print-branch-totals.

           if not ( print-branch-totals = "y" or "n" )
              move "enter (y)es or (n)o only!!!" to mess
              perform send-mess
              go to enter-print-branch-totals.

       enter-print-grand-totals.

           move legend-print-grand-totals to legend.
           perform send-legend.

           move "e  a010prt-grandt." to vdu.
           perform screenio.

           if (        up-key ) go to enter-print-branch-totals.
           if (        f1-key ) go to enter-parameters-exit.
           if (        f7-key ) go to enter-parameters-exit.
           if ( not enter-key ) go to enter-print-grand-totals.

           move vdubuf to print-grand-totals.

           if not ( print-grand-totals = "y" or "n" )
              move "enter (y)es or (n)o only!!!" to mess
              perform send-mess
              go to enter-print-grand-totals.

       local-print.
           if ext-route-buf not = "40"
              if ext-route-buf not = "50"
                 move "n" to local-print-fg
                 go to enter-accept-yn. 

           if beg-branch = end-branch
              move "n" to local-print-fg
              go to enter-accept-yn. 

       ask-local.
           move "s  a220spool-prompt." to vdu.
           move "spool to local       :"    to vdubuf.
           perform screenio.
           move "spool to local directory? (y/n)" to legend.
           perform send-legend.

           move "s  a050yes-no." to vdu.
           move "(y/n)"          to vdubuf.
           perform screenio.

           move "e  a010local-spool." to vdu.
           perform screenio.
           if vdustatus = "10" go to enter-parameters-exit. 
           if vdustatus = "1u" go to enter-print-grand-totals.
           if vdustatus = "1>" go to enter-parameters-exit.
           if vdustatus not = "00" and not = "1d" go to ask-local.

           if vdubuf not = "y"
              if vdubuf not = "n"
                 go to ask-local.

           move vdubuf to local-print-fg.

       enter-accept-yn.

           move legend-accept-yn to legend.
           perform send-legend.

       enter-parameters-exit.
           exit.

      ******************************************************************
       display-accrual-date section.
      ******************************************************************

           if ( accrual-date not = zeroes )
              go to display-accrual-date-exit.

           move ext-coname-sysdate to alp-date.
           perform convert-alpdate-to-yyyymmdd.
           move date-yyyymmdd to accrual-date.


           move "s  8000accrual-date." to vdu.
           move accrual-date to vdu-date-yyyymmdd.
           perform screenio. 

       display-accrual-date-exit.
           exit.

      *****************************************************************
       display-parameters section.
      *****************************************************************

           if ( group-flag = "y" )
              move "s  a000beg-branch."   to vdu
              move ent-group              to vdubuf
              perform screenio
              move "s  a000end-branch."   to vdu
              move spaces                 to vdubuf
              perform screenio
           else
              move "snnn040beg-branch."   to vdu
              move beg-branch             to vdunum0
              perform screenio
              move "snnn040end-branch."   to vdu
              move end-branch             to vdunum0
              perform screenio.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           if (( beg-branch = zeroes ) and ( end-branch = zeroes ))
              move "s  a160display-user." to vdu
              move "user branch only"     to vdubuf
              perform screenio
           else
              move "s  a160display-user." to vdu
              move spaces                 to vdubuf
              perform screenio.

           move "snnn050beg-pool."        to vdu.
           move beg-pool                  to vdunum0.
           perform screenio.

           move "snnn050end-pool."        to vdu.
           move end-pool                  to vdunum0.
           perform screenio.

           move "s  a010export-yno."      to vdu.
           move export-file-yno           to vdubuf.
           perform screenio.

           move "s  a010prt-class."       to vdu.
           move print-by-class            to vdubuf.
           perform screenio.

           move "s  a010prt-pool."        to vdu.
           move print-by-pool             to vdubuf.
           perform screenio.

           move "s  a010prt-ib."          to vdu.
           move print-loan-type-ib        to vdubuf.
           perform screenio.

           move "s  a010prt-ibx."         to vdu.
           move print-loan-type-ibx       to vdubuf.
           perform screenio.

           move "s  a010prt-pc."          to vdu.
           move print-loan-type-pc        to vdubuf.
           perform screenio.

           move "s  a010prt-pcx."          to vdu.
           move print-loan-type-pcx        to vdubuf.
           perform screenio.

           move "s  a010prt-detail."      to vdu.
           move print-detail-analysis     to vdubuf.
           perform screenio.

           move "s  a010prt-classt."      to vdu.
           move print-class-totals        to vdubuf.
           perform screenio.

           move "s  a010prt-poolt."       to vdu.
           move print-pool-totals         to vdubuf.
           perform screenio.

           move "s  a010prt-brancht."     to vdu.
           move print-branch-totals       to vdubuf.
           perform screenio.

           move "s  a010prt-grandt."      to vdu.
           move print-grand-totals        to vdubuf.
           perform screenio.

       display-parameters-exit.
           exit.

      *****************************************************************
       eo-exec-audit section.
      *****************************************************************

           move ext-eobuf-date2 to accrual-date.

       eo-exec-audit-exit.
           exit.

      ******************************************************************
       scan-gr1-file section.
      ******************************************************************

           move grscan-buf to scan-buf.
           perform call-window-program.
           move scan-buf   to grscan-buf.

           if ( scan-status not = "00"   ) or
              ( grscan-key      = spaces )
              move "xx" to vdustatus
              go to scan-gr1-file-exit.

           move "s  a000beg-branch." to vdu.
           move grscan-key           to vdubuf.
           perform screenio.

           move grscan-key to vdubuf.
           move "00"       to vdustatus.

       scan-gr1-file-exit.
           exit.

      ******************************************************************
       execution-module section.
      ******************************************************************

           if ( group-flag = "n" )
              move beg-branch to br-no
              perform start-br-file
              go to execution-next-branch.

           move ent-group to gr-id.
           perform search-gr-init.

       execution-next-group.

           if ( group-flag = "n" )
              go to execution-next-branch.

           perform search-gr-next.
           if ( wgr-sresult = zeroes )
              go to execution-module-end.

           move wgr-sresult to br-no.
           perform read-br-file.
           if ( io-bad )
              go to io-error.

           go to execution-process-branch.

       execution-next-branch.

           perform read-br-file-next.

           if ( io-bad             ) or
              ( br-no > end-branch )
              go to execution-module-end.

       execution-process-branch.

           perform process-branch-module.

           go to execution-next-group.

       execution-module-end.

           perform print-report-module.

       execution-module-exit.
           exit.

      ******************************************************************
       process-branch-module section.
      ******************************************************************

           move 9999 to progid-sleep-counter.
           if ext-route-buf = "00"
              perform send-progress-indicator.

           move ext-filpath-base           to filpath-base.
           move br-machine                 to filpath-machine.
           move br-data-path               to filpath-data.

           move "b"                        to bw-path-override
                                              ln-path-override
                                              lti-path-override
                                              ltp-path-override.

           perform open-bw1-file.
           perform open-ln1-file.
           perform open-lti1-file.

           move zeroes to hold-lc-code.

           move ln-path-ownbr to ln-ownbr.
           move 0             to ln-acctno.

           perform start-ln1-file.

       process-branch-next-ln.

           perform read-ln1-file-next.
           if ( io-bad ) or ( ln-ownbr not = ln-path-ownbr )
              go to process-branch-module-end.

           if ext-route-buf = "00"
              perform send-progress-indicator.
           perform trap-interrupt.

           if ( ln-dlpool < beg-pool ) or ( ln-dlpool > end-pool )
              go to process-branch-next-ln.

           perform lpstat-display.
           move lps-display-lnno to wss-lps-display-lnno.

           move ln-ssno(1) to bw-ssno.
           perform read-bw1-file.
           if ( io-bad )
              move "*not on file*" to bw-lname
              move spaces          to bw-fname.

           move ln-class to lc-code.
           if ( lc1-key not = hold-lc1-key )
              move lc1-key to hold-lc1-key
              perform read-lc1-file
              if ( io-bad )
                 go to io-error.

           move ln-orgst    to sp-orgst.
           move ln-sprclass to sp-sprclass.
           move ln-subclass to sp-subclass.
           move ln-lawcode  to sp-lawcode.
           perform read-sp1-file
           if ( io-bad )
              go to io-error.

           move spaces to process-acctno-stat.
           perform process-acctno-pc.
           perform process-acctno-ib.

           go to process-branch-next-ln.

       process-branch-module-end.

           perform close-bw1-file.
           perform close-ln1-file.
           perform close-lti1-file.

       process-branch-module-exit.
           exit.

      ******************************************************************
       process-acctno-ib section.
      ******************************************************************

           if ( gp-ib-accruals-done not = "y" )
              go to process-acctno-ib-exit.

           if ( lc-servacct = "y" )
              go to process-acctno-ib-exit.

      **************************************************
      *    skip loans paidoff
      *    or in p&l
      *    or in rescission
      *    or loans that have been rescinded
      *    or voided loans
      **************************************************
      
           if ( ln-curbal     = 0      ) or
              ( ln-pldate not = 0      ) or
              ( ln-plcd not   = spaces ) or
              ( ln-pocd-inrescission   ) or
              ( ln-pocd-rescind        ) or
              ( ln-pocd       = "**"   )
              go to process-acctno-ib-exit.

           if ( sp-max-ib-acr-days = 0 )
              go to process-acctno-ib-exit.

      **************************************************
      *    used to always skip repo accounts on ib accrual, from a60
      *    forward.  automotive credit wanted to take earnings till
      *    account charged off mercury, skip repo accounts:
      **************************************************

           if ( gp-ib-accrue-earn-on-repo not = "y"        ) and
              ( ln-repocd                     = "x" or "s" )
              go to process-acctno-ib-exit.

           move accrual-date to ibpc-date.
           perform ibpc-test.

           if (  ibpc-fg           = "i"  ) or
              (( ibpc-fg           = "p" )  and
               ( ibpc-expired-flag = "e" ))
              next sentence
           else
              go to process-acctno-ib-exit.

      * valid record to process  

           move "xx" to process-acctno-stat.
           move "ib" to current-report-type.
           perform load-report-ib-fields.
           perform update-report-file.

      * check for ib exceptions  

           if ( load-ib-max not = spaces )
              move "ibx" to current-report-type
              perform load-report-ibx-fields
              perform update-report-file.

       process-acctno-ib-exit.
           exit.

      ******************************************************************
       process-acctno-pc section.
      ******************************************************************

      * skip loans in rescission and loans that have been rescinded

           if ( ln-pocd-inrescission ) or
              ( ln-pocd-rescind      )
              go to process-acctno-pc-exit.

           if ( ln-pocd = "**" )
              move ln-poffdate to num-date
              if ( ln-salefin = "y" )
                 move ln-purdate  to sys-date
              else
                 move ln-loandate to sys-date.

      * skip loan if not deleted(voided) this month:

           if (  ln-pocd = "**"                )  and
              (( num-yr not = accrual-date-yy )  or
               ( num-mo not = accrual-date-mm ))
              go to process-acctno-pc-exit.

      * skip loan if booked and deleted(voided) this month:

           if ( ln-pocd = "**" ) and
              ( num-yr = s-yy  ) and
              ( num-mo = s-mm  )
              go to process-acctno-pc-exit.

           if ( ln-servacct       = "y"    ) and
              ( ln-prior-unearned = zeroes ) and
              ( ln-nonacrern      = zeroes )
              go to process-acctno-pc-exit.

           if ( ln-pldate     not = zeroes ) and
              ( ln-prior-unearned = zeroes ) and
              ( ln-nonacrern      = zeroes )
              go to process-acctno-pc-exit.

           if ( ln-curbal         = zeroes ) and
              ( ln-prior-unearned = zeroes ) and
              ( ln-nonacrern      = zeroes )
              go to process-acctno-pc-exit.

      *************************************************************
      * jtg corrected logic above for gp-incl-oth-in-earned flag.
      *     needed additional test for when account has
      *     ln-anticern(6) and gp-incl-oth-in-earned = "n", still
      *     need to take an accrual of loan costs.
      *************************************************************

           perform compute-earnings.

           if ( ln-loantype       = "i"  ) and
              ( ln-pocd       not = "**" ) and
              ( earn-tot-anticern = 0    ) and
              ( earn-tot-anticadj = 0    )
              if ( gp-incl-oth-in-earned = "y" )
                 go to process-acctno-pc-exit
              else
              if ( ln-anticern(6) = 0  and ln-anticern(4) = 0)
                 go to process-acctno-pc-exit.

      * valid record to process  

           move "xx" to process-acctno-stat.
           move "pc" to current-report-type.
           perform load-report-pc-fields.
           perform update-report-file.
      
           if load-pc-max not = spaces
              move "pcx" to current-report-type
              perform load-report-pcx-fields
              perform update-report-file.

       process-acctno-pc-exit.
           exit.

      ******************************************************************
       load-calc-addon-earnings section.
      ******************************************************************

           move zeroes to calc-addon-earnings.

           move ln-ownbr        to lti-brno
                                   qlti1-wbeg-brno
                                   qlti1-wend-brno.
           move ln-acctno       to lti-acctno
                                   qlti1-wbeg-acctno
                                   qlti1-wend-acctno.
           move 0               to lti-seqno
                                   qlti1-wbeg-seqno.
           move all "9"         to qlti1-wend-seqno.

           perform start-lti1-file.

       load-calc-addon-earn-next.

           perform read-lti1-file-next.
           if ( io-bad )
              go to load-calc-addon-earn-exit.

           if (lti-brno not = lti-path-ownbr) or
               (lti-acctno not = ln-acctno)
              go to load-calc-addon-earn-exit.

           if not (lti-int-rebate = spaces or "c")
              go to load-calc-addon-earn-next.

           if ( lti-insprem < 0 )
              move -1 to one
           else
              move +1 to one.

           move lti-orig-1stpydate   to aern-ln-orig-1stpydate.
           move lti-antic-start-date to aern-ln-loandate
                                        aern-ln-purdate
                                        aern-ln-intdate.

           subtract lti-prior-nodef from ln-totnodef
                                 giving aern-ln-totnodef.

           move lti-anticterm  to aern-ln-anticterm.
           move "n"           to aern-ln-salefin.
           move lti-int-rebate to aern-ln-rebate.

           multiply one by lti-anticern(1) giving aern-ln-anticern.
           multiply one by lti-anticadj(1) giving aern-ln-anticadj.
           multiply one by lti-ufern   (1) giving aern-ln-ufern.
           multiply one by lti-accern  (1) giving aern-ln-accern.

           move accrual-date         to aern-enddate.
           move ln-last-accrual-date to aern-last-accrual-date.
           move "y"                  to aern-addon-fg.
           move 2                    to aern.

           perform compute-aernings.

           multiply one by aern-earnings(2).
           add aern-earnings(2) to calc-addon-earnings
                                   lti-accern(1).

      *kc->move aern-earnings(2) to lti-int-last-earned.
      *kc->perform rewrite-lti1-file.

           go to load-calc-addon-earn-next.

       load-calc-addon-earn-exit.
           exit.

      ******************************************************************
       load-report-ib-fields section.
      ******************************************************************

           move spaces to
                load-ib-display-lnno        load-ib-lname
                load-ib-fname               load-ib-max.

           move zeroes to
                load-ib-from-date           load-ib-int-since-lstpy
                load-ib-intbal              load-ib-true-accrue
                load-ib-accrued.

           move zeroes to max-ib-interest.

           move wss-lps-display-lnno to load-ib-display-lnno.
           move bw-lname             to load-ib-lname.
           move bw-fname             to load-ib-fname.

           perform load-report-ib-fields-std.      

       load-report-ib-fields-exit.
           exit.

      ******************************************************************
       load-report-ib-fields-std section.
      ******************************************************************

           if ( ln-intpdth-date = 0      )
              move ln-intdate          to load-ib-from-date
           else
              move ln-intpdth-date     to load-ib-from-date.

      * the ib accrual(ac1ern) has this logic to stop the ib accrual
      * and this report needs to match.

      * #1271
           if sp-delq-ib-age-alp not = " "
              if sp-delq-ib-age not = 0
                 move accrual-date to ageing-date
                 perform compute-contractual
                 if contractual >= sp-delq-ib-age
                    move sp-delq-ib-max-days to sp-max-ib-acr-days.

      *************************************************************
      * this routine will establish the maximum interest assess
      * from the last accrued date, per sp-max-ib-acr-days:
      *
      *         re: move 1 to indu-max-accrue
      *************************************************************

           if ( sp-max-ib-acr-days not = 0      )
              if ( ln-intpdth-date = 0      )
                 move ln-intdate      to ndte-date calc-intpdth-date
              else
                 move ln-intpdth-date to ndte-date calc-intpdth-date
              end-if
              move sp-max-ib-acr-days  to ndte-hold
              perform increment-days
              move ndte-date           to rate-date
              move ln-makercd(1)       to rate-makercd
              move "py"                to rate-lptrcd
              move 1                   to indu-max-accrue
              perform set-py-rate
              move indu-interest       to max-ib-interest.

      *************************************************************
      *    this routine will establish the accrued interest assess
      *    from the last accrued date
      *
      *         re: move 0 to indu-max-accrue
      *************************************************************

           move accrual-date           to rate-date.
           move ln-makercd(1)          to rate-makercd.
           move "py"                   to rate-lptrcd.
           move zeroes                 to indu-max-accrue.
           perform set-py-rate.
           move indu-interest          to load-ib-int-since-lstpy.
           move ln-intbal              to load-ib-intbal.

           compute load-ib-true-accrue = ln-intbal + indu-interest.

           if ( sp-max-ib-acr-days not = 999             ) and
              ( load-ib-true-accrue    > max-ib-interest )
              move max-ib-interest     to load-ib-accrued
              move "*"                 to load-ib-max
           else
              move load-ib-true-accrue to load-ib-accrued.

           move ln-actioncd2           to load-ib-actioncd2.

       load-report-ib-fd-std-exit.
           exit.

      ******************************************************************
       load-report-ibx-fields section.
      ******************************************************************

      * normal account ib exception setup for accrual

           move spaces to
                load-ibx-display-lnno       load-ibx-lname
                load-ibx-fname.

           move spaces to
                load-ibx-int-pdth-date      load-ibx-int-since-lstpy
                load-ibx-int-owed           load-ibx-true-accrue
                load-ibx-income-accrue      load-ibx-balance
                load-ibx-contr-pdth-date    load-ibx-contr-status.

           move wss-lps-display-lnno    to load-ibx-display-lnno.
           move bw-lname                to load-ibx-lname.
           move bw-fname                to load-ibx-fname.
           move calc-intpdth-date       to load-ibx-int-pdth-date.
           move load-ib-int-since-lstpy to load-ibx-int-since-lstpy.
           move load-ib-intbal          to load-ibx-int-owed.
           move load-ib-true-accrue     to load-ibx-true-accrue.
           move load-ib-accrued         to load-ibx-income-accrue.
           move ln-curbal               to load-ibx-balance.

           move ln-1stpydate            to pdth-date-work.
           perform paid-thru-calculation.
           move pdth-date-full          to load-ibx-contr-pdth-date.

           move accrual-date            to ageing-date.
           perform compute-contractual.
           move contractual             to load-ibx-contr-status.
           move ln-actioncd2            to load-ibx-actioncd2.

       load-report-ibx-flds-exit.
           exit.

      ******************************************************************
       load-report-pc-fields section.
      ******************************************************************

           move spaces to
                load-pc-display-lnno        load-pc-lname
                load-pc-fname               load-pc-max.

           move zeroes to
                load-pc-anterm              load-pc-antic
                load-pc-unearn              load-pc-int
                load-pc-addon               load-pc-srv
                load-pc-mfee                load-pc-disc
                load-pc-other               load-pc-cash-earn
                load-pc-ending.

           move wss-lps-display-lnno    to load-pc-display-lnno.
           move bw-lname                to load-pc-lname.
           move bw-fname                to load-pc-fname.

      * compute accrual earnings 

           if ( ln-pocd = "**" )
              move zeroes               to earn-tot-unearn
           else
              move accrual-date         to aern-enddate
              move ln-last-accrual-date to aern-last-accrual-date
              perform compute-aernings
              if aern-erndelq-stop = "r"
                 move "r" to load-pc-max
              end-if
              move aern-earnings(1)     to load-pc-int
              move aern-earnings(3)     to load-pc-srv
              move aern-earnings(4)     to load-pc-mfee
              move aern-earnings(5)     to load-pc-disc
              move aern-earnings(6)     to load-pc-other
              move 0                    to load-pc-addon
              if ( ln-addon-fg   = "y"           ) and
                 ( ln-int-rebate = spaces or "c" )
                 perform load-calc-addon-earnings
                 move calc-addon-earnings to load-pc-addon.

           move ln-anticterm            to load-pc-anterm.
           move ln-nonacrern            to load-pc-cash-earn.
           move ln-prior-unearned       to load-pc-unearn.

           compute load-pc-antic         = earn-tot-anticadj
                                         + earn-tot-anticern.

           compute load-pc-ending        = earn-tot-unearn
                                         - load-pc-int
                                         - load-pc-addon
                                         - load-pc-srv
                                         - load-pc-disc.

           if not (gp-incl-oth-in-earned-n or gp-incl-oth-in-earned-r)
              subtract load-pc-other from load-pc-ending.

      * regacc does not want to include mfee (they use to amortize
      * participation expense
           if not (gp-incl-oth-in-earned-r)
              subtract load-pc-mfee from load-pc-ending.

           move ln-actioncd2            to load-pc-actioncd2.

       load-report-pc-fields-exit.
           exit.

      ******************************************************************
       load-report-pcx-fields section.
      ******************************************************************

           move spaces to
                load-pcx-display-lnno        load-pcx-lname
                load-pcx-fname.           

           move zeroes to
                load-pcx-anterm              load-pcx-antic
                load-pcx-unearn              load-pcx-int
                load-pcx-addon               load-pcx-srv
                load-pcx-mfee                load-pcx-disc
                load-pcx-other               load-pcx-rollback   
                load-pcx-contractual.
      
           move wss-lps-display-lnno    to load-pcx-display-lnno.
           move bw-lname                to load-pcx-lname.
           move bw-fname                to load-pcx-fname.

      * compute accrual earnings 

           if ( ln-pocd = "**" )
              move zeroes               to earn-tot-unearn
           else
              move accrual-date         to aern-enddate
              move ln-last-accrual-date to aern-last-accrual-date
              perform compute-aernings
              move aern-earnings(1)     to load-pcx-int
              move aern-earnings(3)     to load-pcx-srv
              move aern-earnings(4)     to load-pcx-mfee
              move aern-earnings(5)     to load-pcx-disc
              move aern-earnings(6)     to load-pcx-other
              move 0                    to load-pcx-addon
              if ( ln-addon-fg   = "y"           ) and
                 ( ln-int-rebate = spaces or "c" )
                   perform load-calc-addon-earnings
                   move calc-addon-earnings to load-pcx-addon.

           move ln-anticterm            to load-pcx-anterm.
           move contractual             to load-pcx-contractual.

           compute load-pcx-antic         = earn-tot-anticadj
                                         + earn-tot-anticern.

           compute load-pcx-rollback = load-pcx-int + 
                                       load-pcx-srv +
                                       load-pcx-mfee +
                                       load-pcx-disc +
                                       load-pcx-other.

           if (gp-incl-oth-in-earned-n or gp-incl-oth-in-earned-r)
             subtract load-pcx-other from   load-pcx-rollback.

      * regacc does not want to include mfee (they use to amortize
      * participation expense 
           if (gp-incl-oth-in-earned-r)                         
              subtract load-pcx-mfee from load-pcx-rollback.

           move ln-actioncd2            to load-pcx-actioncd2.

       load-report-pcx-fields-exit.
           exit.

      ******************************************************************
       print-report-module section.
      ******************************************************************
           if (local-print-fg = "n")
              go to prm-branch-routine.

           perform close-gr1-file.
           perform close-br-file.
           perform open-gr1-file.
           perform open-br-file.

           if ( group-flag = "n" )
              move beg-branch to br-no
              perform start-br-file
              go to prm-execution-next-branch.

           move ent-group to gr-id.
           perform search-gr-init.

       prm-execution-next-group.

           if ( group-flag = "n" )
              go to prm-execution-next-branch.

           perform search-gr-next.
           if ( wgr-sresult = zeroes )
              go to print-report-module-end.

           move wgr-sresult to br-no.
           perform read-br-file.
           if ( io-bad )
              go to io-error.

           go to prm-execution-process-branch.

       prm-execution-next-branch.

           perform read-br-file-next.
           if ( io-bad             ) or
              ( br-no > end-branch )
              go to print-report-module-end.

       prm-execution-process-branch.
           perform close-pr-file.
           perform create-spool-dir.

           if ( not stat-valid )
              go to prm-execution-next-group.

      * sub-report-type is 1 thru 4

           move zeroes to sub-report-type.

       prm-branch-next-type.

           add 01 to sub-report-type.
           if ( sub-report-type > 4 )
              go to prm-execution-next-group.

       prm-branch-routine.

           move spaces               to current-report-type.
           move zeroes               to hold-branch.
           move zeroes               to hold-class.
           move zeroes               to page-number-total.
           move zeroes               to page-number-type.
           move progid-text-printing to progid-text.
           move 9999                 to progid-sleep-counter.

      * start 'report' file  

           move spaces to report-type     hold-report-type.
           move zeroes to report-branch   hold-report-branch.
           move zeroes to report-pool     hold-report-pool.
           move zeroes to report-class    hold-report-class.
           move zeroes to report-acctno   hold-report-acctno.

           if ( local-print-fg = "y" )
              if ( sub-report-type = 1 )
                 move "ib"  to report-type   hold-report-type
                 move br-no to report-branch hold-report-branch
              else
              if ( sub-report-type = 2 )
                 move "ibx" to report-type   hold-report-type
                 move br-no to report-branch hold-report-branch
              else
              if ( sub-report-type = 3 )
                 move "pc"  to report-type   hold-report-type
                 move br-no to report-branch hold-report-branch
              else
                 move "pcx" to report-type   hold-report-type
                 move br-no to report-branch hold-report-branch.

           perform start-report-file.

       print-report-record-next.

           perform read-report-file-next.
           if ( io-bad )
              if (local-print-fg = "y")
                 go to prm-execution-next-group
              else
                 go to print-report-module-end.

      *****************************************************************
      *    for doing export file only!
      *    for doing export file with report ("y") is done further
      *    below in the area of printing detail analysis.
      *****************************************************************

           if ( export-file-yno = "o" )   *> for detail records
              if ( report-grand-total  ) or
                 ( report-branch-total ) or
                 ( report-pool-total   ) or
                 ( report-class-total  )
                 go to print-report-record-next
              else
                 move "y" to records-found
                 perform update-export-file
                 go to print-report-record-next.

           if ( local-print-fg = "y" )
              if ( report-branch not = br-no )
                 go to prm-branch-next-type       
              else
              if ( report-type = "ib"  and sub-report-type not = 1 ) or
                 ( report-type = "ibx" and sub-report-type not = 2 ) or
                 ( report-type = "pc"  and sub-report-type not = 3 ) or
                 ( report-type = "pcx" and sub-report-type not = 4 )
                 go to prm-branch-next-type.

           move report-branch to br-no.
           perform send-progress-indicator.
           perform trap-interrupt.

      * check report type  

           if (( print-loan-type-ib  = "n"  ) and ( report-type-ib )) or
              (( print-loan-type-ibx = "n"  ) and ( report-type-ibx)) or
              (( print-loan-type-pc  = "n"  ) and ( report-type-pc )) or
              (( print-loan-type-pcx = "n"  ) and ( report-type-pcx)) 
              go to print-report-record-next.

      * check report options 

           if ( report-grand-total )
              if ( print-grand-totals = "y" )
                 go to print-report-record-valid
              else
                 go to print-report-record-next.

           if ( report-branch-total )
              if ( print-branch-totals = "y" )
                 go to print-report-record-valid
              else
                 go to print-report-record-next.

           if ( report-pool-total )
              if ( print-pool-totals = "y" )
                 go to print-report-record-valid
              else
                 go to print-report-record-next.

           if ( report-class-total )
              if ( print-class-totals = "y" )
                 go to print-report-record-valid
              else
                 go to print-report-record-next.

           if ( export-file-yno = "y" or "o" )   *> for detail records
              move "y" to records-found
              perform update-export-file.

           if ( print-detail-analysis = "y"  )
              go to print-report-record-valid.

           go to print-report-record-next.

       print-report-record-valid.

           perform print-report-page-break.

           if ( report-type-ib  ) perform print-report-ib.
           if ( report-type-ibx ) perform print-report-ibx.
           if ( report-type-pc  ) perform print-report-pc.
           if ( report-type-pcx ) perform print-report-pcx.

           move "y" to records-found.

           go to print-report-record-next.

       print-report-module-end.

           if (local-print-fg = "n")
               perform print-report-ranges.

       print-report-module-exit.
           exit.

      ******************************************************************
       print-report-page-break section.
      ******************************************************************

      * page break only when going to print a 'detail' record after
      * just printing a 'total' record; or if a change of report type

           if (( not current-report-type-ib ) and ( report-type-ib )) or
              (( not current-report-type-ibx) and ( report-type-ibx)) or
              (( not current-report-type-pc ) and ( report-type-pc )) or
              (( not current-report-type-pcx) and (report-type-pcx))

              move report-type to current-report-type
              move 99          to ln-count
              move zeroes      to page-number-type
              go to print-report-pg-break-end.

      ******************************************************************
      *  do not change ln-count if printing a 'total' record.
      *  allow totals to print on same page.
      ******************************************************************
      
           if ( report-grand-total  ) or
              ( report-branch-total ) or
              ( report-pool-total   ) or
              ( report-class-total  )
              go to print-report-pg-break-end.

      ******************************************************************
      * at this point the current record is for a 'detail' record.
      * if the prevoius record was a 'total' record, do a page break.
      ******************************************************************
      
           if ( hold-report-grand-total  ) or
              ( hold-report-branch-total ) or
              ( hold-report-pool-total   ) or
              ( hold-report-class-total  )
              move 99 to ln-count.

       print-report-pg-break-end.

           move report-key to hold-report-key.

       print-report-pg-break-exit.
           exit.

      ******************************************************************
       print-report-ranges section.
      ******************************************************************

           if ( no-records-found )
              go to print-report-ranges-exit.

           move 99     to ln-count.
           move 1      to ln-feed.
           move spaces to detail-line.
           perform write-detail-line.

      
           if ( group-flag = "y" )
              move "group id name                 : " to r01-prompt
              move ent-group                          to r01-text
              move 2                                  to ln-feed
              perform write-detail-line
           else
              move "beginning branch number       : " to r01-prompt
              move beg-branch                         to r01-numeric
              move 2                                  to ln-feed
              perform write-detail-line
              move "ending    branch number       : " to r01-prompt
              move end-branch                         to r01-numeric
              move 1                                  to ln-feed
              perform write-detail-line.

           move "beginning pool id             : "    to r01-prompt.
           move beg-pool                              to r01-numeric.
           move 2                                     to ln-feed.
           perform write-detail-line.

           move "ending    pool id             : "    to r01-prompt.
           move beg-pool                              to r01-numeric.
           move 1                                     to ln-feed.
           perform write-detail-line.

           move "accrual date (mm/dd/yy)       : "    to r01-prompt.
           move accrual-date to date-yyyymmdd.
           perform convert-yyyymmdd-to-mmddyy.
           move date-mmddyy                           to r01-date.
           move 2                                     to ln-feed.
           perform write-detail-line.

           move "report breakdown by class     : "    to r01-prompt.
           move print-by-class                        to r01-text.
           move 2                                     to ln-feed.
           perform write-detail-line.

           move "report breakdown by pool      : "    to r01-prompt.
           move print-by-pool                         to r01-text.
           move 1                                     to ln-feed.
           perform write-detail-line.

           move "report interest bearing       : "    to r01-prompt.
           move print-loan-type-ib                    to r01-text.
           move 2                                     to ln-feed.
           perform write-detail-line.

           move "report interest bearing except: "    to r01-prompt.
           move print-loan-type-ibx                   to r01-text.
           move 1                                     to ln-feed.
           perform write-detail-line.

           move "report precompute             : "    to r01-prompt.
           move print-loan-type-pc                    to r01-text.
           move 1                                     to ln-feed.
           perform write-detail-line.

           move "report precompute except:     : "    to r01-prompt.
           move print-loan-type-pcx                   to r01-text.
           move 1                                     to ln-feed.
           perform write-detail-line.

           move "print detail analysis         : "    to r01-prompt.
           move print-detail-analysis                 to r01-text.
           move 2                                     to ln-feed.
           perform write-detail-line.

           move "print class  totals           : "    to r01-prompt.
           move print-class-totals                    to r01-text.
           move 1                                     to ln-feed.
           perform write-detail-line.

           move "print pool   totals           : "    to r01-prompt.
           move print-pool-totals                     to r01-text.
           move 1                                     to ln-feed.
           perform write-detail-line.

           move "print branch totals           : "    to r01-prompt.
           move print-branch-totals                   to r01-text.
           move 1                                     to ln-feed.
           perform write-detail-line.

           move "print grand  totals           : "    to r01-prompt.
           move print-grand-totals                    to r01-text.
           move 1                                     to ln-feed.
           perform write-detail-line.

           move "starting time                 : "    to r01-prompt.
           move starting-time                         to r01-text.
           move 2                                     to ln-feed.
           perform write-detail-line.

           move "ending   time                 : "    to r01-prompt.
           perform get-time.
           move time-edit                             to r01-text.
           move 1                                     to ln-feed.
           perform write-detail-line.

       print-report-ranges-exit.
           exit.

      ******************************************************************
       print-report-ib section.
      ******************************************************************

           if ( report-grand-total  )  or
              ( report-branch-total )  or
              ( report-pool-total   )  or
              ( report-class-total  )
              if ( print-detail-analysis = "y" )
                 go to print-report-ib-total
              else
                 go to print-report-ib-summary.

       print-report-ib-detail.

           move report-ib-display-lnno     to wss-lps-display-lnno.
           move wss-lps-number-7status     to d01-ib-display-lnno.

           string report-ib-lname, ", ", report-ib-fname
                  delimited by "  "      into d01-ib-name.

           move report-ib-from-date        to date-yyyymmdd.
           perform convert-yyyymmdd-to-mmddyy.
           move date-mmddyy                to d01-ib-from.
           move report-ib-int-since-lstpy  to d01-ib-int-since-lstpy.
           move report-ib-intbal           to d01-ib-intbal.
           move report-ib-true-accrue      to d01-ib-true-accrue.
           move report-ib-accrued          to d01-ib-accrued.
           move report-ib-max              to d01-ib-max.

           move 1 to ln-feed.
           perform write-detail-line.

           go to print-report-ib-exit.

       print-report-ib-summary.

      * total summary only 

           if ( report-grand-total )
              move "****"                  to s01-ib-branchx
           else
              move report-branch           to s01-ib-branch.

           if ( report-branch-total )
              move "****"                  to s01-ib-poolx
           else
              move report-pool             to s01-ib-pool.

           if ( report-pool-total )
              move "**"                    to s01-ib-classx
           else
              move report-class            to s01-ib-class.

           move report-ib-number-accts     to s01-ib-number-accts.
           move "#"                        to s01-ib-number-accts-fg.
           move report-ib-int-since-lstpy  to s01-ib-int-since-lstpy.
           move report-ib-intbal           to s01-ib-intbal.
           move report-ib-true-accrue      to s01-ib-true-accrue.
           move report-ib-accrued          to s01-ib-accrued.

           move 2 to ln-feed.
           perform write-detail-line.

           go to print-report-ib-exit.

       print-report-ib-total.

           if ( report-grand-total )
              move "grand totals         " to t01-ib-prompt
           else
           if ( report-branch-total )
              move "branch totals        " to t01-ib-prompt
           else
           if ( report-pool-total )
              move "pool totals          " to t01-ib-prompt
           else
              move "class totals         " to t01-ib-prompt.

           move report-ib-number-accts     to t01-ib-number-accts.
           move "#"                        to t01-ib-number-accts-fg.
           move report-ib-int-since-lstpy  to t01-ib-int-since-lstpy.
           move report-ib-intbal           to t01-ib-intbal.
           move report-ib-true-accrue      to t01-ib-true-accrue.
           move report-ib-accrued          to t01-ib-accrued.

           move 2 to ln-feed.
           perform write-detail-line.

           go to print-report-ib-exit.

       print-report-ib-exit.
           exit.

      ******************************************************************
       print-report-ibx section.
      ******************************************************************

           if ( report-grand-total  )  or
              ( report-branch-total )  or
              ( report-pool-total   )  or
              ( report-class-total  )
              if ( print-detail-analysis = "y" )
                 go to print-report-ibx-total
              else
                 go to print-report-ibx-summary.

       print-report-ibx-detail.

           move report-ibx-display-lnno    to wss-lps-display-lnno.
           move wss-lps-number-7status     to d02-ibx-display-lnno.

           string report-ibx-lname, ", ", report-ibx-fname
                  delimited by "  "      into d02-ibx-name.

           move report-ibx-int-pdth-date   to date-yyyymmdd.
           perform convert-yyyymmdd-to-mmddyy.
           move date-mmddyy                to d02-ibx-int-pdth.
           move report-ibx-int-since-lstpy to d02-ibx-int-since-lstpy.
           move report-ibx-int-owed        to d02-ibx-int-owed.
           move report-ibx-true-accrue     to d02-ibx-true-accrue.
           move report-ibx-income-accrue   to d02-ibx-income-accrue.
           move report-ibx-balance         to d02-ibx-balance.
           move report-ibx-contr-pdth-date to date-yyyymmdd.
           perform convert-yyyymmdd-to-mmddyy.
           move date-mmddyy                to d02-ibx-cont-pdth.
           move report-ibx-contr-status    to d02-ibx-cont-status.

           move 1 to ln-feed.
           perform write-detail-line.

           go to print-report-ibx-exit.

       print-report-ibx-summary.

      * total summary only 

           if ( report-grand-total )
              move "****"                  to s02-ibx-branchx
           else
              move report-branch           to s02-ibx-branch.

           if ( report-branch-total )
              move "****"                  to s02-ibx-poolx
           else
              move report-pool             to s02-ibx-pool.

           if ( report-pool-total )
              move "**"                    to s02-ibx-classx
           else
              move report-class            to s02-ibx-class.

           move report-ibx-number-accts    to s02-ibx-number-accts.
           move "#"                        to s02-ibx-number-accts-fg.
           move report-ibx-int-since-lstpy to s02-ibx-int-since-lstpy.
           move report-ibx-int-owed        to s02-ibx-int-owed.
           move report-ibx-true-accrue     to s02-ibx-true-accrue.
           move report-ibx-income-accrue   to s02-ibx-income-accrue.
           move report-ibx-balance         to s02-ibx-balance.

           move 2 to ln-feed.
           perform write-detail-line.

           go to print-report-ibx-exit.

       print-report-ibx-total.

           if ( report-grand-total )
              move "grand totals          " to t02-ibx-prompt
           else
           if ( report-branch-total )
              move "branch totals         " to t02-ibx-prompt
           else
           if ( report-pool-total )
              move "pool totals           " to t02-ibx-prompt
           else
              move "class totals          " to t02-ibx-prompt.

           move report-ibx-number-accts     to t02-ibx-number-accts.
           move "#"                         to t02-ibx-number-accts-fg.
           move report-ibx-int-since-lstpy  to t02-ibx-int-since-lstpy.
           move report-ibx-int-owed         to t02-ibx-int-owed.
           move report-ibx-true-accrue      to t02-ibx-true-accrue.
           move report-ibx-income-accrue    to t02-ibx-income-accrue.
           move report-ibx-balance          to t02-ibx-balance.

           move 2 to ln-feed.
           perform write-detail-line.

           go to print-report-ibx-exit.

       print-report-ibx-exit.
           exit.

      ******************************************************************
       print-report-pc section.
      ******************************************************************

           if ( report-grand-total  )  or
              ( report-branch-total )  or
              ( report-pool-total   )  or
              ( report-class-total  )
              if ( print-detail-analysis = "y" )
                 go to print-report-pc-total
              else
                 go to print-report-pc-summary.

       print-report-pc-detail.

           move report-pc-display-lnno     to wss-lps-display-lnno.
           move wss-lps-number-7status     to d03-pc-display-lnno.

           string report-pc-lname, ", ", report-pc-fname
                  delimited by "  "      into d03-pc-name.

           move report-pc-anterm           to d03-pc-anterm.
           move report-pc-antic            to d03-pc-antic.
           move report-pc-unearn           to d03-pc-unearn.
           move report-pc-int              to d03-pc-int.
           move report-pc-addon            to d03-pc-addon.
           move report-pc-srv              to d03-pc-srv.
           move report-pc-mfee             to d03-pc-mfee.
           move report-pc-disc             to d03-pc-disc.
           move report-pc-other            to d03-pc-other.
           move report-pc-cash-earn        to d03-pc-cash-earn.
           move report-pc-ending           to d03-pc-ending.

           move 1 to ln-feed.
           perform write-detail-line.

           go to print-report-pc-exit.

       print-report-pc-summary.

      * total summary only 

           if ( report-grand-total )
              move "****"                  to s03-pc-branchx
           else
              move report-branch           to s03-pc-branch.

           if ( report-branch-total )
              move "****"                  to s03-pc-poolx
           else
              move report-pool             to s03-pc-pool.

           if ( report-pool-total )
              move "**"                    to s03-pc-classx
           else
              move report-class            to s03-pc-class.

           move report-pc-number-accts     to s03-pc-number-accts.
           move "#"                        to s03-pc-number-accts-fg.
           move report-pc-unearn           to s03-pc-unearn.
           move report-pc-int              to s03-pc-int.
           move report-pc-addon            to s03-pc-addon.
           move report-pc-srv              to s03-pc-srv.
           move report-pc-mfee             to s03-pc-mfee.
           move report-pc-disc             to s03-pc-disc.
           move report-pc-other            to s03-pc-other.
           move report-pc-cash-earn        to s03-pc-cash-earn.
           move report-pc-ending           to s03-pc-ending.

           move 2 to ln-feed.
           perform write-detail-line.

           go to print-report-pc-exit.

       print-report-pc-total.

           if ( report-grand-total )
              move "grand totals          " to t03-pc-prompt
           else
           if ( report-branch-total )
              move "branch totals         " to t03-pc-prompt
           else
           if ( report-pool-total )
              move "pool totals           " to t03-pc-prompt
           else
              move "class totals          " to t03-pc-prompt.

           move report-pc-number-accts     to t03-pc-number-accts.
           move "#"                        to t03-pc-number-accts-fg.
           move report-pc-unearn           to t03-pc-unearn.
           move report-pc-addon            to t03-pc-addon.
           move report-pc-mfee             to t03-pc-mfee.
           move report-pc-other            to t03-pc-other.
           move report-pc-ending           to t03-pc-ending.

           move 2 to ln-feed.
           perform write-detail-line.

           move report-pc-int              to t04-pc-int.
           move report-pc-srv              to t04-pc-srv.
           move report-pc-disc             to t04-pc-disc.
           move report-pc-cash-earn        to t04-pc-cash-earn.

           move 1 to ln-feed.
           perform write-detail-line.

           go to print-report-pc-exit.

       print-report-pc-exit.
           exit.

      ******************************************************************
       print-report-pcx section.
      ******************************************************************

           if ( report-grand-total  )  or
              ( report-branch-total )  or
              ( report-pool-total   )  or
              ( report-class-total  )
              if ( print-detail-analysis = "y" )
                 go to print-report-pcx-total
              else
                 go to print-report-pcx-summary.

       print-report-pcx-detail.

           move report-pcx-display-lnno     to wss-lps-display-lnno.
           move wss-lps-number-7status      to d04-pcx-display-lnno.

           string report-pcx-lname, ", ", report-pcx-fname
                  delimited by "  "      into d04-pcx-name.

           move report-pcx-anterm           to d04-pcx-anterm.
           move report-pcx-antic            to d04-pcx-antic.
           move report-pcx-unearn           to d04-pcx-unearn.
           move report-pcx-int              to d04-pcx-int.
           move report-pcx-addon            to d04-pcx-addon.
           move report-pcx-srv              to d04-pcx-srv.
           move report-pcx-mfee             to d04-pcx-mfee.
           move report-pcx-disc             to d04-pcx-disc.
           move report-pcx-other            to d04-pcx-other.
           move report-pcx-rollback         to d04-pcx-rollback.  
           move report-pcx-contractual      to d04-pcx-contractual.
           move 1 to ln-feed.
           perform write-detail-line.

           go to print-report-pcx-exit.

       print-report-pcx-summary.

      * total summary only 

           if ( report-grand-total )
              move "****"                  to s04-pcx-branchx
           else
              move report-branch           to s04-pcx-branch.

           if ( report-branch-total )
              move "****"                  to s04-pcx-poolx
           else
              move report-pool             to s04-pcx-pool.

           if ( report-pool-total )
              move "**"                    to s04-pcx-classx
           else
              move report-class            to s04-pcx-class.

           move report-pcx-number-accts    to s04-pcx-number-accts.
           move "#"                        to s04-pcx-number-accts-fg.
           move report-pcx-unearn          to s04-pcx-unearn.
           move report-pcx-int             to s04-pcx-int.
           move report-pcx-addon           to s04-pcx-addon.
           move report-pcx-srv             to s04-pcx-srv.
           move report-pcx-mfee            to s04-pcx-mfee.
           move report-pcx-disc            to s04-pcx-disc.
           move report-pcx-other           to s04-pcx-other.
           move report-pcx-rollback        to s04-pcx-rollback.   

           move 2 to ln-feed.
           perform write-detail-line.

           go to print-report-pcx-exit.

       print-report-pcx-total.

           if ( report-grand-total )
              move "grand totals          " to t03-pcx-prompt
           else
           if ( report-branch-total )
              move "branch totals         " to t03-pcx-prompt
           else
           if ( report-pool-total )
              move "pool totals           " to t03-pcx-prompt
           else
              move "class totals          " to t03-pcx-prompt.

           move report-pcx-number-accts   to t03-pcx-number-accts.
           move "#"                       to t03-pcx-number-accts-fg.
           move report-pcx-unearn         to t03-pcx-unearn.
           move report-pcx-addon          to t03-pcx-addon.
           move report-pcx-mfee           to t03-pcx-mfee.
           move report-pcx-other          to t03-pcx-other.

           move 2 to ln-feed.
           perform write-detail-line.

           move report-pcx-int              to t04-pcx-int.
           move report-pcx-srv              to t04-pcx-srv.
           move report-pcx-disc             to t04-pcx-disc.
           move report-pcx-rollback         to t04-pcx-rollback.  

           move 1 to ln-feed.
           perform write-detail-line.

           go to print-report-pcx-exit.

       print-report-pcx-exit. 
           exit.

      ******************************************************************
      *
      *    u p d a t e - e x p o r t - f i l e
      *
      * in  : report-rec
      * from: print-report-record-next.
      * desc: using report-rec, which are detail account records, will
      *       create export files.
      ******************************************************************
       update-export-file section.

           if not ( report-type-ib  )
              go to update-export-file-ibx.

           move spaces                     to ac2day-ib-rec.
           move report-branch              to ac2day-ib-brno.

           move report-ib-display-lnno     to wss-lps-display-lnno.
           move wss-lps-number             to ac2day-ib-acctno.
           move wss-lps-7status            to ac2day-ib-7status.
           move wss-lps-class              to ac2day-ib-class.

           string report-ib-lname, ", ", report-ib-fname
                  delimited by "  "      into ac2day-ib-name.

           move report-ib-from-date        to ac2day-ib-from-date.
           move report-ib-int-since-lstpy  to ac2day-ib-int-since-lstpy.
           move report-ib-intbal           to ac2day-ib-intbal.
           move report-ib-true-accrue      to ac2day-ib-true-accrue.
           move report-ib-accrued          to ac2day-ib-accrued.
           move report-ib-max              to ac2day-ib-max.
           move report-ib-actioncd2        to ac2day-ib-actioncd2.
           move x"0d"                      to ac2day-ib-cr.
           move x"0a"                      to ac2day-ib-lf.

           add 1 to ac2day-ib-key.
           perform write-ac2day-ib-file.
           if ( io-bad )
              go to io-error.

       update-export-file-ibx.

           if not ( report-type-ibx  )
              go to update-export-file-pc.

           move spaces                     to ac2day-ibx-rec.
           move report-branch              to ac2day-ibx-brno.

           move report-ib-display-lnno     to wss-lps-display-lnno.
           move wss-lps-number             to ac2day-ibx-acctno.
           move wss-lps-7status            to ac2day-ibx-7status.
           move wss-lps-class              to ac2day-ibx-class.

           string report-ibx-lname, ", ", report-ibx-fname
                  delimited by "  "      into ac2day-ibx-name.

           move report-ibx-int-pdth-date   to ac2day-ibx-int-pdth-date.
           move report-ibx-int-since-lstpy to
                                             ac2day-ibx-int-since-lstpy.
           move report-ibx-int-owed        to ac2day-ibx-int-owed.
           move report-ibx-true-accrue     to ac2day-ibx-true-accrue.
           move report-ibx-income-accrue   to ac2day-ibx-income-accrue.
           move report-ibx-balance         to ac2day-ibx-balance.
           move report-ibx-contr-pdth-date to ac2day-ibx-cont-pdth-date.
           move report-ibx-contr-status    to ac2day-ibx-cont-status.
           move report-ibx-actioncd2       to ac2day-ibx-actioncd2.
           move x"0d"                      to ac2day-ibx-cr.
           move x"0a"                      to ac2day-ibx-lf.

           add 1 to ac2day-ibx-key.
           perform write-ac2day-ibx-file.

           if ( io-bad )
              go to io-error.

       update-export-file-pc.

           if not ( report-type-pc  )
              go to update-export-file-pcx.

           move spaces                     to ac2day-pc-rec.
           move report-branch              to ac2day-pc-brno.

           move report-ib-display-lnno     to wss-lps-display-lnno.
           move wss-lps-number             to ac2day-pc-acctno.
           move wss-lps-7status            to ac2day-pc-7status.
           move wss-lps-class              to ac2day-pc-class.

           string report-pc-lname, ", ", report-pc-fname
                  delimited by "  "      into ac2day-pc-name.

           move report-pc-anterm           to ac2day-pc-anterm.
           move report-pc-antic            to ac2day-pc-antic.
           move report-pc-unearn           to ac2day-pc-unearn.
           move report-pc-int              to ac2day-pc-int.
           move report-pc-addon            to ac2day-pc-addon.
           move report-pc-srv              to ac2day-pc-srv.
           move report-pc-mfee             to ac2day-pc-mfee.
           move report-pc-disc             to ac2day-pc-disc.
           move report-pc-other            to ac2day-pc-other.
           move report-pc-cash-earn        to ac2day-pc-cash-earn.
           move report-pc-ending           to ac2day-pc-ending.
           move report-pc-actioncd2        to ac2day-pc-actioncd2.
           move x"0d"                      to ac2day-pc-cr.
           move x"0a"                      to ac2day-pc-lf.

           add 1 to ac2day-pc-key.
           perform write-ac2day-pc-file.
           if ( io-bad )
              go to io-error.

       update-export-file-pcx.

           if not ( report-type-pcx  )
              go to update-export-file-exit.

           move spaces                     to ac2day-pcx-rec.
           move report-branch              to ac2day-pcx-brno.

           move report-ib-display-lnno     to wss-lps-display-lnno.
           move wss-lps-number             to ac2day-pcx-acctno.
           move wss-lps-7status            to ac2day-pcx-7status.
           move wss-lps-class              to ac2day-pcx-class.

           string report-pcx-lname, ", ", report-pcx-fname
                  delimited by "  "      into ac2day-pcx-name.

           move report-pcx-anterm          to ac2day-pcx-anterm.
           move report-pcx-antic           to ac2day-pcx-antic.
           move report-pcx-unearn          to ac2day-pcx-unearn.
           move report-pcx-int             to ac2day-pcx-int.
           move report-pcx-addon           to ac2day-pcx-addon.
           move report-pcx-srv             to ac2day-pcx-srv.
           move report-pcx-mfee            to ac2day-pcx-mfee.
           move report-pcx-disc            to ac2day-pcx-disc.
           move report-pcx-other           to ac2day-pcx-other.
           move report-pcx-rollback        to ac2day-pcx-rollback.
           move report-pcx-contractual     to ac2day-pcx-contractual.
           move report-pcx-actioncd2       to ac2day-pcx-actioncd2.
           move x"0d"                      to ac2day-pcx-cr.
           move x"0a"                      to ac2day-pcx-lf.

           add 1 to ac2day-pcx-key.
           perform write-ac2day-pcx-file.

           if ( io-bad )
              go to io-error.

       update-export-file-exit.
           exit.

      ******************************************************************
       update-report-file section.
      ******************************************************************

           move current-report-type to hold-report-type.
           move ln-ownbr            to hold-report-branch.
           move zeroes              to hold-report-pool.
           move ln-class            to hold-report-class.
           move ln-acctno           to hold-report-acctno.

           if ( print-by-pool = "y" )
              move ln-dlpool      to hold-report-pool.

      * update: detail 

           perform update-report-record.

      * update: class  

           move 99999999 to hold-report-acctno.
           perform update-report-record.

      * update: pool 

           move 9999     to hold-report-class.

           if ( print-by-pool = "y" )
              perform update-report-record.

      * update: branch 

           move 999999   to hold-report-pool.
           perform update-report-record.

      * update: grand  

           move 99999    to hold-report-branch.
           perform update-report-record.

       update-report-file-exit.
           exit.

      ******************************************************************
       update-report-record section.
      ******************************************************************

           move hold-report-key to report-key.
           perform read-report-file.
           if ( io-bad )
              perform update-report-record-clear
              perform write-report-file
              if ( io-bad )
                 go to io-error.

           if ( report-type-ib   ) perform update-report-record-ib.
           if ( report-type-ibx  ) perform update-report-record-ibx.
           if ( report-type-pc   ) perform update-report-record-pc.
           if ( report-type-pcx  ) perform update-report-record-pcx.

       update-report-record-exit.
           exit.

      ******************************************************************
       update-report-record-clear section.
      ******************************************************************

           move spaces          to report-rec.
           move hold-report-key to report-key.

           if ( report-type-ib )
              move zeroes to
                   report-ib-from-date       report-ib-int-since-lstpy
                   report-ib-intbal          report-ib-true-accrue
                   report-ib-accrued         report-ib-max
                   report-ib-number-accts.

           if ( report-type-ibx )
              move zeroes to
                   report-ibx-int-pdth-date
                   report-ibx-int-since-lstpy
                   report-ibx-int-owed
                   report-ibx-true-accrue
                   report-ibx-income-accrue
                   report-ibx-balance
                   report-ibx-contr-pdth-date
                   report-ibx-contr-status  report-ibx-number-accts.

           if ( report-type-pc )
              move zeroes to
                   report-pc-anterm          report-pc-antic
                   report-pc-unearn          report-pc-int
                   report-pc-addon           report-pc-srv
                   report-pc-mfee            report-pc-disc
                   report-pc-other           report-pc-cash-earn
                   report-pc-ending          report-pc-number-accts.

           if ( report-type-pcx )
              move zeroes to 
                   report-pcx-anterm          report-pcx-antic
                   report-pcx-unearn          report-pcx-int
                   report-pcx-addon           report-pcx-srv
                   report-pcx-mfee            report-pcx-disc
                   report-pcx-other           report-pcx-rollback 
                   report-pcx-number-accts.

       update-report-rec-clr-exit.
           exit.

      ******************************************************************
       update-report-record-ib section.
      ******************************************************************

           if ( report-grand-total  ) or
              ( report-branch-total ) or
              ( report-pool-total ) or
              ( report-class-total  )
              go to update-report-record-ib-skip.

           move load-ib-display-lnno     to report-ib-display-lnno.
           move load-ib-lname            to report-ib-lname.
           move load-ib-fname            to report-ib-fname.
           move load-ib-from-date        to report-ib-from-date.
           move load-ib-max              to report-ib-max.
           move load-ib-actioncd2        to report-ib-actioncd2.

       update-report-record-ib-skip.

           add  load-ib-int-since-lstpy  to report-ib-int-since-lstpy.
           add  load-ib-intbal           to report-ib-intbal.
           add  load-ib-true-accrue      to report-ib-true-accrue.
           add  load-ib-accrued          to report-ib-accrued.
           add  01                       to report-ib-number-accts.

      * updating to 'report' file  

           perform rewrite-report-file.

       update-report-record-ib-exit.
           exit.

      ******************************************************************
       update-report-record-ibx section.
      ******************************************************************

           if ( report-grand-total  ) or
              ( report-branch-total ) or
              ( report-pool-total ) or
              ( report-class-total  )
              go to update-report-rc-ibx-skip.

           move load-ibx-display-lnno    to report-ibx-display-lnno.
           move load-ibx-lname           to report-ibx-lname.
           move load-ibx-fname           to report-ibx-fname.
           move load-ibx-int-pdth-date   to report-ibx-int-pdth-date.
           move load-ibx-contr-pdth-date to report-ibx-contr-pdth-date.
           move load-ibx-contr-status    to report-ibx-contr-status.
           move load-ibx-actioncd2       to report-ibx-actioncd2.

       update-report-rc-ibx-skip.

           add load-ibx-int-since-lstpy to report-ibx-int-since-lstpy.
           add load-ibx-int-owed        to report-ibx-int-owed.
           add load-ibx-true-accrue     to report-ibx-true-accrue.
           add load-ibx-income-accrue   to report-ibx-income-accrue.
           add load-ibx-balance         to report-ibx-balance.

           add  01 to report-ibx-number-accts.

      * updating to 'report' file  

           perform rewrite-report-file.

       update-report-rc-ibx-exit.
           exit.

      ******************************************************************
       update-report-record-pc section.
      ******************************************************************

           if ( report-grand-total  ) or
              ( report-branch-total ) or
              ( report-pool-total   ) or
              ( report-class-total  )
              go to update-report-rec-pc-skip.

           move load-pc-display-lnno      to report-pc-display-lnno.
           move load-pc-lname             to report-pc-lname.
           move load-pc-fname             to report-pc-fname.
           move load-pc-anterm            to report-pc-anterm.
           move load-pc-actioncd2         to report-pc-actioncd2.

       update-report-rec-pc-skip.

           add  load-pc-antic             to report-pc-antic.
           add  load-pc-unearn            to report-pc-unearn.
           add  load-pc-int               to report-pc-int.
           add  load-pc-addon             to report-pc-addon.
           add  load-pc-srv               to report-pc-srv.
           add  load-pc-mfee              to report-pc-mfee.
           add  load-pc-disc              to report-pc-disc.
           add  load-pc-other             to report-pc-other.
           add  load-pc-cash-earn         to report-pc-cash-earn.
           add  load-pc-ending            to report-pc-ending.
           add  01                        to report-pc-number-accts.

      * updating to 'report' file  

           perform rewrite-report-file.

       update-report-rec-pc-exit.
           exit.

      ******************************************************************
       update-report-record-pcx section.
      ******************************************************************

           if ( report-grand-total  ) or
              ( report-branch-total ) or
              ( report-pool-total   ) or
              ( report-class-total  )
              go to update-report-rec-pcx-skip.

           move load-pcx-display-lnno    to report-pcx-display-lnno.
           move load-pcx-lname           to report-pcx-lname.
           move load-pcx-fname           to report-pcx-fname.
           move load-pcx-anterm          to report-pcx-anterm.
           move load-pcx-contractual     to report-pcx-contractual.
           move load-pcx-actioncd2       to report-pcx-actioncd2.

       update-report-rec-pcx-skip.

           add  load-pcx-antic             to report-pcx-antic.
           add  load-pcx-unearn            to report-pcx-unearn.
           add  load-pcx-int               to report-pcx-int.
           add  load-pcx-addon             to report-pcx-addon.
           add  load-pcx-srv               to report-pcx-srv.
           add  load-pcx-mfee              to report-pcx-mfee.
           add  load-pcx-disc              to report-pcx-disc.
           add  load-pcx-other             to report-pcx-other.
           add  load-pcx-rollback          to report-pcx-rollback.
           add  01                        to report-pcx-number-accts.

      * updating to 'report' file  

           perform rewrite-report-file.

       update-report-rec-pcx-exit.
           exit.

      ******************************************************************
       write-detail-line section.
      ******************************************************************

           add ln-feed to ln-count.

           if ( ln-count > ext-coname-rpt-lines )
              perform write-page-header-lines.

           move detail-line to pr-rec.
           perform write-pr-rec.

           move 1 to ln-feed.
           move spaces to detail-line.

       write-detail-line-exit.
           exit.

      ******************************************************************
       write-page-header-lines section.
      ******************************************************************

           move 01 to ln-count.
           add  01 to page-number-total.
           add  01 to page-number-type
           perform get-time.

           if ( print-detail-analysis = "n" )
              go to write-page-header-continue.

           if ( report-branch not = hold-branch ) and
              ( not report-grand-total          )
              move report-branch to br-no hold-branch
              perform read-br-file
              if ( io-bad )
                 go to io-error.

           if ( report-pool = zeroes )
              move "* no pool id for loans *"    to cd-desc
           else
              if ( report-pool not = hold-pool ) and
                 ( not report-branch-total     )
                 move "pl"                          to cd-type
                 move report-pool                   to cd-pl-poolid
                 move spaces                        to cd-pl-filler
                 perform read-cd1-file
                 if ( io-bad )
                    move "* not on file *         " to cd-desc.

           if ( report-class not = hold-class ) and
              ( not report-pool-total         )
              move report-class to lc-code hold-class
              perform read-lc1-file
              if ( io-bad )
                 go to io-error.

       write-page-header-continue.

      * 01
           move ext-coname-sysdate               to h01-date.
           move t-hh                             to h01-hour.
           move t-mm                             to h01-minute.
           move ext-coname-coname                to h01-name.
           call "c$justify"                   using h01-name "c".
           move page-number-type                 to h01-page-type.
           move page-number-total                to h01-page-total.

           move header-line-01                   to pr-rec.
           move 99                               to ln-feed.
           perform write-pr-rec.

      * 02
           move ext-coname-user-id               to h02-userid.
           move accrual-date                     to date-yyyymmdd.
           perform convert-yyyymmdd-to-mmddyy.
           move date-mmddyy                      to h02-accrual-date.

           if ( current-report-type-ib )
              move header-title-type-ib          to h02-title
           else
           if ( current-report-type-ibx )
              move header-title-type-ibx         to h02-title
           else
           if ( current-report-type-pc )
              move header-title-type-pc          to h02-title
           else
           if ( current-report-type-pcx )
              move header-title-type-pcx         to h02-title
           else
              move spaces                        to h02-title.

           call "c$justify"                   using h02-title "c".

           move header-line-02                   to pr-rec.
           add  01                               to ln-count.
           move 01                               to ln-feed.
           perform write-pr-rec.

      * 03
           if ( print-detail-analysis = "y" )
              move br-no                         to h03-branch
              move br-name                       to h03-branch-desc
           else
              move "****"                        to h03-branchx
              move "*all branches*"              to h03-branch-desc.

           move header-line-03                   to pr-rec.
           add  01                               to ln-count.
           move 01                               to ln-feed.
           perform write-pr-rec.

      * 04
           if ( print-detail-analysis = "y" )
              move report-pool                   to h04-pool
              move cd-desc                       to h04-pool-desc
           else
              move "*****"                       to h04-poolx
              move "*all pools*"                 to h04-pool-desc.

           if ( print-by-pool = "y" )
               move header-line-04               to pr-rec
               add  01                           to ln-count
               move 01                           to ln-feed
               perform write-pr-rec.

      * 05
           if ( print-detail-analysis = "y" )
              move report-class                  to h05-class
              move lc-desc                       to h05-class-desc
           else
              move "**"                          to h05-classx
              move "*all classes*"               to h05-class-desc.

           move spaces                           to h05-mtd-non.

           if ( current-report-type-pc      ) and
              ( print-detail-analysis = "y" )
              move "mtd-non"                     to h05-mtd-non.

           move header-line-05                   to pr-rec.
           add  01                               to ln-count.
           move 01                               to ln-feed.
           perform write-pr-rec.

      * 06
           if ( report-type-ib )
              if ( print-detail-analysis = "y" )
                 move header-line-ib-01          to pr-rec
              else
                 move header-line-ib-01-summary  to pr-rec
           else
           if ( report-type-ibx )
              if ( print-detail-analysis = "y" )
                 move header-line-ibx-01         to pr-rec
              else
                 move header-line-ibx-01-summary to pr-rec
           else
           if ( report-type-pc )
              if ( print-detail-analysis = "y" )
                 move header-line-pc-01          to pr-rec
              else
                 move header-line-pc-01-summary  to pr-rec 
           else
           if ( report-type-pcx )
              if ( print-detail-analysis = "y" )
                 move header-line-pcx-01          to pr-rec
              else
                 move header-line-pcx-01-summary  to pr-rec.

           add  01 to ln-count.
           move 01 to ln-feed.
           perform write-pr-rec.

      * 07
           if ( report-type-ib )
              if ( print-detail-analysis = "y" )
                 move header-line-ib-02          to pr-rec
              else
                 move header-line-ib-02-summary  to pr-rec
           else
           if ( report-type-ibx )
              if ( print-detail-analysis = "y" )
                 move header-line-ibx-02         to pr-rec
              else
                 move header-line-ibx-02-summary to pr-rec
           else
           if ( report-type-pc )
              if ( print-detail-analysis = "y" )
                 move header-line-pc-02          to pr-rec
              else
                 move header-line-pc-02-summary  to pr-rec 
           else
           if ( report-type-pcx )
              if ( print-detail-analysis = "y" )
                 move header-line-pcx-02          to pr-rec
              else
                 move header-line-pcx-02-summary  to pr-rec.

           add  01 to ln-count.
           move 01 to ln-feed.
           perform write-pr-rec.

      * 08
           if ( report-type-ib )
              if ( print-detail-analysis = "y" )
                 move all "="                    to pr-rec
              else
                 move header-line-ib-03-summary  to pr-rec
           else
           if ( report-type-ibx )
              if ( print-detail-analysis = "y" )
                 move all "="                    to pr-rec
              else
                 move header-line-ibx-03-summary to pr-rec
           else
           if ( report-type-pc )
              if ( print-detail-analysis = "y" )
                 move all "="                    to pr-rec
              else
                 move header-line-pc-03-summary  to pr-rec 
           else
           if ( report-type-pcx )
              if ( print-detail-analysis = "y" )
                 move all "="                    to pr-rec
              else
                 move header-line-pcx-03-summary  to pr-rec.

           add  01 to ln-count.
           move 01 to ln-feed.
           perform write-pr-rec.

      * 09
           if ( report-type-ib )
              if ( print-detail-analysis = "y" )
                 move spaces                     to pr-rec
              else
                 move all "="                    to pr-rec
           else
           if ( report-type-ibx )
              if ( print-detail-analysis = "y" )
                 move spaces                     to pr-rec
              else
                 move all "="                    to pr-rec
           else
           if ( report-type-pc )
              if ( print-detail-analysis = "y" )
                 move spaces                     to pr-rec
              else
                 move all "="                    to pr-rec 
           else
           if ( report-type-pcx )
              if ( print-detail-analysis = "y" )
                 move spaces                     to pr-rec
              else
                 move all "="                    to pr-rec.

           if ( pr-rec not = spaces )
              add  01 to ln-count
              move 01 to ln-feed
              perform write-pr-rec.

       write-page-header-ln-end.

           move 01 to ln-feed.

       write-page-header-ln-exit.
           exit.

      ******************************************************************
      *
      *    c o p y - l i b r a r y - s e c t i o n s
      *
      ******************************************************************
       copy "libgb/ageing.cpy".
       copy "libgb/brsecure.cpy".
       copy "libgb/dater.cpy".
       copy "libgb/eospool.cpy".
       copy "libgb/gpenv.cpy".
       copy "libgb/grserch.cpy".
       copy "libgb/grwork.cpy".
       copy "libgb/progid.cpy".
       copy "libgb/rebate.cpy".
       copy "libgb/screen.cpy".
       copy "libgb/sppdth.cpy".
       copy "libgb/timeio.cpy".

       copy "liblp/lpaern.cpy".
       copy "liblp/lpamts.cpy".
       copy "liblp/lpaprs.cpy".
       copy "liblp/lpaprz.cpy".
       copy "liblp/lpcepp.cpy".
       copy "liblp/lpcern.cpy".
       copy "liblp/lpeadj.cpy".
       copy "liblp/lpearn.cpy".
       copy "liblp/lpibpc.cpy".
       copy "liblp/lpitrm.cpy".
       copy "liblp/lpmdte.cpy".
       copy "liblp/lppdue.cpy".
       copy "liblp/lprate.cpy".
       copy "liblp/lpregz.cpy".
       copy "liblp/lpstat.cpy".
       copy "liblp/lpuern.cpy".
       copy "liblp/lpuper.cpy".
       copy "liblp/lpupwk.cpy".
       copy "liblp/lpxdte.cpy".
       copy "libgb/rbact.cpy".
       copy "liblp/setctbl.cpy".
       copy "libwi/pfswindows.cpy".

      *********************************************
       form-reset section.
      *********************************************
           move ext-coname-coname to ac2day-coname.
           move ext-coname-sysdate to ac2day-sysdate.
           display ac2day-screen upon ext-eobuf-window-handle.
           move ac2day-form-fields to wr-buf.
           move ac2day-form-list to wr-list.
           perform call-wrform.


      *********************************************
       setup-link-info section.
      *********************************************
           move ac2day-help--file to help-link-file.
           move ac2day-help--dir  to help-link-dir.

      *********************************************
       vdu-convert-field section.
      *********************************************
           move "00" to stat.
           move vdu-field-to-convert to vdu-alp-field.
           evaluate true
              copy "liblp/ac2day_eva.cpy".
              when other       move   "2a" to stat
           end-evaluate.

      ******************************************************************
       misc-paragraphs section.
      ******************************************************************
       copy "libgb/access.cpy".
       copy "libgb/daccess.cpy".
       copy "libgb/getenv.cpy".
       copy "libgb/getpr.cpy".
       copy "libgb/mktemp.cpy".
       copy "libgb/system.cpy".
       copy "libgb/rmfile.cpy".
       send-legend.
           move legend to wr-buf.
           move legend-list to wr-list.
           perform call-wrform.
       initial-progress-indicator.
           move zeroes                 to progid-branch.
           move "legend:b."            to progid-line-list.
           move pru-lp                 to progid-pru-lp.
           move ext-route-buf          to progid-route-buf.
           move 9999                   to progid-sleep-counter.
           move 250                    to progid-sleep-counter-max.

           perform get-time.
           move time-edit              to progid-time.

           move "b"                    to progid-type.
           move progid-text-processing to progid-text.

       send-progress-indicator.
           move br-no     to progid-branch.
           perform get-time.
           move time-edit to progid-time.
           if ext-route-buf = "00"
              perform progid-display.
       verify-password.
           move " "                          to passwdw-stat.
           move "ac2day"                     to passwdw-pgm.
           move "please enter the password," to passwdw-prompt1.
           move "cl/passwd"                  to form-nam.
           call form-progx using form-path passwdw-buf.
           cancel form-path.

      ******************************************************************
       io-routine section.
      ******************************************************************
       copy "libgb/connect_sql.cpy".
       copy "liblp/lpspgs_sql.cpy".
       copy "liblp/lpltpgs_sql.cpy".
       copy "liblp/lpltigs_sql.cpy".
       copy "liblp/lplngs_sql.cpy".
       copy "liblp/lplcgs_sql.cpy".
       copy "liblp/lpcdgs_sql.cpy".
       copy "liblp/lpbwgs_sql.cpy".
       copy "libgb/gbgrgs_sql.cpy".
       copy "libgb/gbbrgs_sql.cpy".

       copy "libgb/gbprint.cpy".
       copy "libgb/gbbrrn.cpy".
       copy "libgb/gbgr1rn.cpy".
       copy "liblp/lpbw1rn.cpy".
       copy "liblp/lpcd1rn.cpy".
       copy "liblp/lplc1rn.cpy".
       copy "liblp/lpln1rn.cpy".
       copy "liblp/lplti1rn.cpy".
       copy "liblp/lpltp1rn.cpy".
       copy "liblp/lpsp1rn.cpy".
       open-ac2day-ib-file-output.
           perform open-it.
           move ac2day-ib-path to e-file.
           open output ac2day-ib-file.
           if io-fg = 8
              go to open-ac2day-ib-file-output.
           if io-fg = 7
              close ac2day-ib-file
              go to open-ac2day-ib-file-output.
       write-ac2day-ib-file.
           perform write-it.
           move ac2day-ib-path to e-file.
           write ac2day-ib-rec.
           if io-fg = 8
             go to write-ac2day-ib-file.
       close-ac2day-ib-file.
           perform close-it.
           close ac2day-ib-file.

       open-ac2day-ibx-file-output.
           perform open-it.
           move ac2day-ibx-path to e-file.
           open output ac2day-ibx-file.
           if io-fg = 8
              go to open-ac2day-ibx-file-output.
           if io-fg = 7
              close ac2day-ibx-file
              go to open-ac2day-ibx-file-output.
       write-ac2day-ibx-file.
           perform write-it.
           move ac2day-ibx-path to e-file.
           write ac2day-ibx-rec.
           if io-fg = 8
             go to write-ac2day-ibx-file.
       close-ac2day-ibx-file.
           perform close-it.
           close ac2day-ibx-file.

       open-ac2day-pc-file-output.
           perform open-it.
           move ac2day-pc-path to e-file.
           open output ac2day-pc-file.
           if io-fg = 8
              go to open-ac2day-pc-file-output.
           if io-fg = 7
              close ac2day-pc-file
              go to open-ac2day-pc-file-output.
       write-ac2day-pc-file.
           perform write-it.
           move ac2day-pc-path to e-file.
           write ac2day-pc-rec.
           if io-fg = 8
             go to write-ac2day-pc-file.
       close-ac2day-pc-file.
           perform close-it.
           close ac2day-pc-file.

       open-ac2day-pcx-file-output.
           perform open-it.
           move ac2day-pcx-path to e-file.
           open output ac2day-pcx-file.
           if io-fg = 8
              go to open-ac2day-pcx-file-output.
           if io-fg = 7
              close ac2day-pcx-file
              go to open-ac2day-pcx-file-output.
       write-ac2day-pcx-file.
           perform write-it.
           move ac2day-pcx-path to e-file.
           write ac2day-pcx-rec.
           if io-fg = 8
             go to write-ac2day-pcx-file.
       close-ac2day-pcx-file.
           perform close-it.
           close ac2day-pcx-file.

       open-report-file-output.
           perform open-it.
           move report-ifn to e-file.
           open output report-file.
           if ( io-locked )
              go to open-report-file-output.
           if ( io-already-open )
              close report-file
              go to open-report-file-output.
           unlock report-file.
       open-report-file.
           perform open-it.
           move report-ifn to e-file.
           open i-o report-file.
           if ( io-locked )
              go to open-report-file.
           if ( io-already-open )
              close report-file
              go to open-report-file.
           unlock report-file.
       read-report-file.
           perform read-it.
           move report-ifn to e-file.
           move report-key to e-keyx.
           read report-file.
           if ( io-locked )
              go to read-report-file.
       read-report-file-next.
           perform read-it.
           move report-ifn to e-file.
           move report-key to e-keyx.
           read report-file next.
           if ( io-locked )
              go to read-report-file-next.
       start-report-file.
           perform start-it.
           move report-ifn to e-file.
           move report-key to e-keyx.
           start report-file key not < report-key.
           unlock report-file.
       write-report-file.
           perform write-it.
           move report-ifn to e-file.
           move report-key to e-keyx.
           write report-rec.
           if ( io-locked )
              go to write-report-file.
           unlock report-file.
       rewrite-report-file.
           perform rewrite-it.
           move report-ifn to e-file.
           move report-key to e-keyx.
           rewrite report-rec.
           if ( io-locked )
              go to rewrite-report-file.
           unlock report-file.
       close-report-file.
           perform close-it.
           close report-file.
       copy "libgb/ferrors.cpy".
           move "e" to ext-eobuf-errcd.

      ******************************************************************
       end-program section.
      ******************************************************************
       end-run.

           perform close-files.

           move report-path to access-buf.
           perform remove-workfile.

           if ( exit-prior-to-execution )
              go to exit-prog.

           if ( ext-route-buf = "00" )
              perform pru-out.

           if ( no-records-found         ) and
              ( ext-route-buf = "00" or "70" )
              move "no records found for selected range" to mess
              perform send-mess.

       exit-prog.

           if ( sv-beg-branch not = 9999 )
              if ( ext-eobuf-group-fg = "y"  )
                 move sv-beg-branch     to beg-branch
                 move sv-end-branch     to end-branch
                 move sv-group-flag     to group-flag
                 move sv-ent-group      to ent-group
                 move sv-local-print-fg to local-print-fg
              else
                 move sv-beg-branch to beg-branch
                 move sv-end-branch to end-branch.

           if ext-route-buf = "00"
              move exit-path  to form-nam
              move form-path to form-pathname.

       stop-run.
           if ext-route-buf = "00" or "40" or "50"
              display spaces line 0 col 0 erase screen
              destroy ac2day-screen
              destroy ext-eobuf-window-handle.
           exit program.
