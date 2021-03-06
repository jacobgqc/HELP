
     '
     'HELP Acknowledgement and reference screen program
     '
VIEW PRINT 1 TO 25
COLOR 12, 0: KEY OFF: CLS : LOCATE 1, 1, O
1000 PRINT "                  Hydrologic Evaluation of Landfill Performance ": COLOR 14, 0
1010 PRINT
1030 PRINT "               ���     ���    ����������    ���          ��������� "
1040 PRINT "                �       �      �       �     �            �      � "
1050 PRINT "                �       �      �             �            �      � "
1051 PRINT "                �       �      �    �        �            �      � "
1060 PRINT "                ���������      ������        �            �������� "
1061 PRINT "                �       �      �             �            �   "
1062 PRINT "                �       �      �             �            �   "
1070 PRINT "               ���     ���    ����������    ���������    ���  "
1080 PRINT : COLOR 6, 0
1090 PRINT "                          Version 3.07   1 November 1997"
1100 PRINT : COLOR 10, 0
1110 PRINT "                 ���������������������������������������������ͻ"
1120 PRINT "                 �"; : COLOR 11: PRINT "                Developed by                 "; : COLOR 10: PRINT "�"
1150 PRINT "                 �"; : COLOR 11: PRINT "          Environmental Laboratory           "; : COLOR 10: PRINT "�"
1160 PRINT "                 �"; : COLOR 11: PRINT "      USAE Waterways Experiment Station      "; : COLOR 10: PRINT "�"
1180 PRINT "                 �"; : COLOR 11: PRINT "      Vicksburg, Mississippi 39180-6199      "; : COLOR 10: PRINT "�"
1121 PRINT "                 �"; : COLOR 11: PRINT "                                             "; : COLOR 10: PRINT "�"
1151 PRINT "                 �"; : COLOR 11: PRINT "                    for                      "; : COLOR 10: PRINT "�"
1161 PRINT "                 �"; : COLOR 11: PRINT " USEPA Risk Reduction Engineering Laboratory "; : COLOR 10: PRINT "�"
1181 PRINT "                 �"; : COLOR 11: PRINT "           Cincinnati, Ohio 45268            "; : COLOR 10: PRINT "�"
1190 PRINT "                 ���������������������������������������������ͼ"
1200 PRINT : COLOR 15, 0
1220 PRINT "                           Press any key to continue...";
1250 A$ = INKEY$: IF A$ = "" GOTO 1250
COLOR 10, 0
3000 PRINT "������������������������������������������������������������������������������Ŀ"
3010 PRINT "�"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "�"
3030 PRINT "�"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "�"
3040 PRINT "�"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "�"
3151 PRINT "�"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "�"
3050 PRINT "�"; : COLOR 14: PRINT "                             DISCLAIMER STATEMENT                             "; : COLOR 10: PRINT "�"
3051 PRINT "�"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "�"
3060 PRINT "�"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "�"
3061 PRINT "�"; : COLOR 11: PRINT "          This program is furnished by the U.S. Government and is             "; : COLOR 10: PRINT "�"
3062 PRINT "�"; : COLOR 11: PRINT "     accepted and used by the recipient with the express understanding        "; : COLOR 10: PRINT "�"
3070 PRINT "�"; : COLOR 11: PRINT "     that the U.S. Government gives no warranties, expressed or implied,      "; : COLOR 10: PRINT "�"
3080 PRINT "�"; : COLOR 11: PRINT "     concerning the accuracy, reliability, usability, or suitability for      "; : COLOR 10: PRINT "�"
3090 PRINT "�"; : COLOR 11: PRINT "     any particular purpose of the information and data contained in this     "; : COLOR 10: PRINT "�"
3100 PRINT "�"; : COLOR 11: PRINT "     program or furnished in connection therewith.  The United States of      "; : COLOR 10: PRINT "�"
3110 PRINT "�"; : COLOR 11: PRINT "     America shall be under no liability whatsoever to any person by          "; : COLOR 10: PRINT "�"
3120 PRINT "�"; : COLOR 11: PRINT "     reason of any use made thereof.  This program belongs to the U.S.        "; : COLOR 10: PRINT "�"
3150 PRINT "�"; : COLOR 11: PRINT "     Government; therefore, the recipient further agrees not to assert        "; : COLOR 10: PRINT "�"
3160 PRINT "�"; : COLOR 11: PRINT "     any proprietary rights therein, or to represent this program to          "; : COLOR 10: PRINT "�"
3180 PRINT "�"; : COLOR 11: PRINT "     anyone as other than a U.S. Government program.                          "; : COLOR 10: PRINT "�"
3121 PRINT "�"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "�"
3161 PRINT "�"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "�"
3181 PRINT "�"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "�"
3190 PRINT "�"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "�"
3220 PRINT "��������������������������������������������������������������������������������"
COLOR 15, 0
3230 PRINT "                           Press any key to continue...";
2250 A$ = INKEY$: IF A$ = "" GOTO 2250
COLOR 10, 0
2000 PRINT "������������������������������������������������������������������������������Ŀ"
2161 PRINT "�"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "�"
2040 PRINT "�"; : COLOR 14: PRINT "                                   PREFACE                                    "; : COLOR 10: PRINT "�"
2180 PRINT "�"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "�"
2051 PRINT "�"; : COLOR 11: PRINT "       This program was developed by US Army Engineer Waterways Experiment    "; : COLOR 10: PRINT "�"
2060 PRINT "�"; : COLOR 11: PRINT "   Station under funding from the USEPA.  The program was written to provide  "; : COLOR 10: PRINT "�"
2061 PRINT "�"; : COLOR 11: PRINT "   technical support and resources for the RCRA and CERCLA programs.  This    "; : COLOR 10: PRINT "�"
2062 PRINT "�"; : COLOR 11: PRINT "   is the third major release of the HELP model.  Version 3 was developed     "; : COLOR 10: PRINT "�"
2070 PRINT "�"; : COLOR 11: PRINT "   under the direction of Dr. Paul R. Schroeder.  The user interface was      "; : COLOR 10: PRINT "�"
2080 PRINT "�"; : COLOR 11: PRINT "   written by Dr. Nadim Aziz of Clemson University.  The model is based in    "; : COLOR 10: PRINT "�"
2090 PRINT "�"; : COLOR 11: PRINT "   part on USDA-ARS models:  CREAMS, EPIC, SWRRB, SPUR, HYDRO-17 and WGEN.    "; : COLOR 10: PRINT "�"
2110 PRINT "�"; : COLOR 11: PRINT "       HELP was designed to perform water balance analyses for design and     "; : COLOR 10: PRINT "�"
2120 PRINT "�"; : COLOR 11: PRINT "   evaluation of land disposal facilities.  Use of the HELP model requires    "; : COLOR 10: PRINT "�"
2150 PRINT "�"; : COLOR 11: PRINT "   good understanding of landfill design practices.  To insure valid results  "; : COLOR 10: PRINT "�"
2160 PRINT "�"; : COLOR 11: PRINT "   the user should review the overall design prior to using the model.        "; : COLOR 10: PRINT "�"
2151 PRINT "�"; : COLOR 11: PRINT "       Design guidance for landfills is available in the following:           "; : COLOR 10: PRINT "�"
2200 PRINT "�"; : COLOR 11: PRINT "          USEPA.  1989.  Technical Guidance Document:  Final Covers on        "; : COLOR 10: PRINT "�"
2001 PRINT "�"; : COLOR 11: PRINT "   Hazardous Waste Landfills and Surface Impoundments.  EPA/530-SW-89-047.    "; : COLOR 10: PRINT "�"
2002 PRINT "�"; : COLOR 11: PRINT "   Office of Solid Waste and Emergency Response, Washington, DC.              "; : COLOR 10: PRINT "�"
2181 PRINT "�"; : COLOR 11: PRINT "          USEPA.  1987.  Minimum Technology Guidance on Double Liner Systems  "; : COLOR 10: PRINT "�"
2190 PRINT "�"; : COLOR 11: PRINT "   for Landfills and Surface Impoundments - Design, Construction and          "; : COLOR 10: PRINT "�"
2121 PRINT "�"; : COLOR 11: PRINT "   Operation.  EPA/530-SW-85-014.  NTIS PB 87-151072.  OSWER, Washington, DC. "; : COLOR 10: PRINT "�"
2183 PRINT "�"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "�"
2220 PRINT "��������������������������������������������������������������������������������"
COLOR 15, 0
2230 PRINT "                           Press any key to continue...";
3250 A$ = INKEY$: IF A$ = "" GOTO 3250
     END








