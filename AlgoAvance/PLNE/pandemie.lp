\ SCIP STATISTICS
\   Problem name     : pandemie.zpl
\   Variables        : 258 (258 binary, 0 integer, 0 implicit integer, 0 continuous)
\   Constraints      : 295
Minimize
 Obj: +10 x#1#1 +12 x#1#2 +14 x#1#3 +5 x#1#4 +6 x#2#1 +10 x#2#2 +10 x#2#3 +4 x#2#4 +12 x#3#1 +12 x#3#2
      +16 x#3#3 +6 x#3#4 +6 y#1#1#2 +6 y#1#1#3 +8 y#1#1#4 +7 y#1#2#3 +8 y#1#2#4 +5 y#1#3#4 +6 y#2#1#2
      +6 y#2#1#3 +8 y#2#1#4 +7 y#2#2#3 +8 y#2#2#4 +5 y#2#3#4 +6 y#3#1#2 +6 y#3#1#3 +8 y#3#1#4 +7 y#3#2#3
      +8 y#3#2#4 +5 y#3#3#4
Subject to
 site_1: +1 x#1#1 +1 x#2#1 +1 x#3#1 >= +1
 site_2: +1 x#1#2 +1 x#2#2 +1 x#3#2 >= +1
 site_3: +1 x#1#3 +1 x#2#3 +1 x#3#3 >= +1
 site_4: +1 x#1#4 +1 x#2#4 +1 x#3#4 >= +1
 eq_1: +1 x#1#1 +1 x#1#2 +1 x#1#3 +1 x#1#4 <= +2
 eq_2: +1 x#2#1 +1 x#2#2 +1 x#2#3 +1 x#2#4 <= +2
 eq_3: +1 x#3#1 +1 x#3#2 +1 x#3#3 +1 x#3#4 <= +2
 depc_1_a_0: +1 x#1#1 -1 @@depc_1_xp_0 +1 @@depc_1_xm_1 = +1
 depc_1_b_1: +1 @@depc_1_bp_2 -1 @@depc_1_xp_0 <= +0
 depc_1_c_2: +1 @@depc_1_bm_3 -1 @@depc_1_xm_1 <= +0
 depc_1_d_3: -1 @@depc_1_xp_0 >= +0
 depc_1_e_4: +1 @@depc_1_bm_3 -1 @@depc_1_xm_1 >= +0
 depc_1_f_5: +1 @@depc_1_re_4 +1 @@depc_1_bp_2 +1 @@depc_1_bm_3 = +1
 depc_1_a_6: +1 x#1#2 -1 @@depc_1_xp_5 +1 @@depc_1_xm_6 = +1
 depc_1_b_7: +1 @@depc_1_bp_7 -1 @@depc_1_xp_5 <= +0
 depc_1_c_8: +1 @@depc_1_bm_8 -1 @@depc_1_xm_6 <= +0
 depc_1_d_9: -1 @@depc_1_xp_5 >= +0
 depc_1_e_10: +1 @@depc_1_bm_8 -1 @@depc_1_xm_6 >= +0
 depc_1_f_11: +1 @@depc_1_re_9 +1 @@depc_1_bp_7 +1 @@depc_1_bm_8 = +1
 depc_1_a_12: +1 @@depc_1_re_4 -1 @@depc_1_re_10 >= +0
 depc_1_b_13: +1 @@depc_1_re_9 -1 @@depc_1_re_10 >= +0
 depc_1_c_14: +1 @@depc_1_re_4 +1 @@depc_1_re_9 -1 @@depc_1_re_10 <= +1
 depc_1_t_l: -1 @@depc_1_re_10 +1 y#1#1#2 >= +0
 depc_2_a_15: +1 x#1#1 -1 @@depc_2_xp_11 +1 @@depc_2_xm_12 = +1
 depc_2_b_16: +1 @@depc_2_bp_13 -1 @@depc_2_xp_11 <= +0
 depc_2_c_17: +1 @@depc_2_bm_14 -1 @@depc_2_xm_12 <= +0
 depc_2_d_18: -1 @@depc_2_xp_11 >= +0
 depc_2_e_19: +1 @@depc_2_bm_14 -1 @@depc_2_xm_12 >= +0
 depc_2_f_20: +1 @@depc_2_re_15 +1 @@depc_2_bp_13 +1 @@depc_2_bm_14 = +1
 depc_2_a_21: +1 x#1#3 -1 @@depc_2_xp_16 +1 @@depc_2_xm_17 = +1
 depc_2_b_22: +1 @@depc_2_bp_18 -1 @@depc_2_xp_16 <= +0
 depc_2_c_23: +1 @@depc_2_bm_19 -1 @@depc_2_xm_17 <= +0
 depc_2_d_24: -1 @@depc_2_xp_16 >= +0
 depc_2_e_25: +1 @@depc_2_bm_19 -1 @@depc_2_xm_17 >= +0
 depc_2_f_26: +1 @@depc_2_re_20 +1 @@depc_2_bp_18 +1 @@depc_2_bm_19 = +1
 depc_2_a_27: +1 @@depc_2_re_15 -1 @@depc_2_re_21 >= +0
 depc_2_b_28: +1 @@depc_2_re_20 -1 @@depc_2_re_21 >= +0
 depc_2_c_29: +1 @@depc_2_re_15 +1 @@depc_2_re_20 -1 @@depc_2_re_21 <= +1
 depc_2_t_l: -1 @@depc_2_re_21 +1 y#1#1#3 >= +0
 depc_3_a_30: +1 x#1#1 -1 @@depc_3_xp_22 +1 @@depc_3_xm_23 = +1
 depc_3_b_31: +1 @@depc_3_bp_24 -1 @@depc_3_xp_22 <= +0
 depc_3_c_32: +1 @@depc_3_bm_25 -1 @@depc_3_xm_23 <= +0
 depc_3_d_33: -1 @@depc_3_xp_22 >= +0
 depc_3_e_34: +1 @@depc_3_bm_25 -1 @@depc_3_xm_23 >= +0
 depc_3_f_35: +1 @@depc_3_re_26 +1 @@depc_3_bp_24 +1 @@depc_3_bm_25 = +1
 depc_3_a_36: +1 x#1#4 -1 @@depc_3_xp_27 +1 @@depc_3_xm_28 = +1
 depc_3_b_37: +1 @@depc_3_bp_29 -1 @@depc_3_xp_27 <= +0
 depc_3_c_38: +1 @@depc_3_bm_30 -1 @@depc_3_xm_28 <= +0
 depc_3_d_39: -1 @@depc_3_xp_27 >= +0
 depc_3_e_40: +1 @@depc_3_bm_30 -1 @@depc_3_xm_28 >= +0
 depc_3_f_41: +1 @@depc_3_re_31 +1 @@depc_3_bp_29 +1 @@depc_3_bm_30 = +1
 depc_3_a_42: +1 @@depc_3_re_26 -1 @@depc_3_re_32 >= +0
 depc_3_b_43: +1 @@depc_3_re_31 -1 @@depc_3_re_32 >= +0
 depc_3_c_44: +1 @@depc_3_re_26 +1 @@depc_3_re_31 -1 @@depc_3_re_32 <= +1
 depc_3_t_l: -1 @@depc_3_re_32 +1 y#1#1#4 >= +0
 depc_4_a_45: +1 x#1#2 -1 @@depc_4_xp_33 +1 @@depc_4_xm_34 = +1
 depc_4_b_46: +1 @@depc_4_bp_35 -1 @@depc_4_xp_33 <= +0
 depc_4_c_47: +1 @@depc_4_bm_36 -1 @@depc_4_xm_34 <= +0
 depc_4_d_48: -1 @@depc_4_xp_33 >= +0
 depc_4_e_49: +1 @@depc_4_bm_36 -1 @@depc_4_xm_34 >= +0
 depc_4_f_50: +1 @@depc_4_re_37 +1 @@depc_4_bp_35 +1 @@depc_4_bm_36 = +1
 depc_4_a_51: +1 x#1#3 -1 @@depc_4_xp_38 +1 @@depc_4_xm_39 = +1
 depc_4_b_52: +1 @@depc_4_bp_40 -1 @@depc_4_xp_38 <= +0
 depc_4_c_53: +1 @@depc_4_bm_41 -1 @@depc_4_xm_39 <= +0
 depc_4_d_54: -1 @@depc_4_xp_38 >= +0
 depc_4_e_55: +1 @@depc_4_bm_41 -1 @@depc_4_xm_39 >= +0
 depc_4_f_56: +1 @@depc_4_re_42 +1 @@depc_4_bp_40 +1 @@depc_4_bm_41 = +1
 depc_4_a_57: +1 @@depc_4_re_37 -1 @@depc_4_re_43 >= +0
 depc_4_b_58: +1 @@depc_4_re_42 -1 @@depc_4_re_43 >= +0
 depc_4_c_59: +1 @@depc_4_re_37 +1 @@depc_4_re_42 -1 @@depc_4_re_43 <= +1
 depc_4_t_l: -1 @@depc_4_re_43 +1 y#1#2#3 >= +0
 depc_5_a_60: +1 x#1#2 -1 @@depc_5_xp_44 +1 @@depc_5_xm_45 = +1
 depc_5_b_61: +1 @@depc_5_bp_46 -1 @@depc_5_xp_44 <= +0
 depc_5_c_62: +1 @@depc_5_bm_47 -1 @@depc_5_xm_45 <= +0
 depc_5_d_63: -1 @@depc_5_xp_44 >= +0
 depc_5_e_64: +1 @@depc_5_bm_47 -1 @@depc_5_xm_45 >= +0
 depc_5_f_65: +1 @@depc_5_re_48 +1 @@depc_5_bp_46 +1 @@depc_5_bm_47 = +1
 depc_5_a_66: +1 x#1#4 -1 @@depc_5_xp_49 +1 @@depc_5_xm_50 = +1
 depc_5_b_67: +1 @@depc_5_bp_51 -1 @@depc_5_xp_49 <= +0
 depc_5_c_68: +1 @@depc_5_bm_52 -1 @@depc_5_xm_50 <= +0
 depc_5_d_69: -1 @@depc_5_xp_49 >= +0
 depc_5_e_70: +1 @@depc_5_bm_52 -1 @@depc_5_xm_50 >= +0
 depc_5_f_71: +1 @@depc_5_re_53 +1 @@depc_5_bp_51 +1 @@depc_5_bm_52 = +1
 depc_5_a_72: +1 @@depc_5_re_48 -1 @@depc_5_re_54 >= +0
 depc_5_b_73: +1 @@depc_5_re_53 -1 @@depc_5_re_54 >= +0
 depc_5_c_74: +1 @@depc_5_re_48 +1 @@depc_5_re_53 -1 @@depc_5_re_54 <= +1
 depc_5_t_l: -1 @@depc_5_re_54 +1 y#1#2#4 >= +0
 depc_6_a_75: +1 x#1#3 -1 @@depc_6_xp_55 +1 @@depc_6_xm_56 = +1
 depc_6_b_76: +1 @@depc_6_bp_57 -1 @@depc_6_xp_55 <= +0
 depc_6_c_77: +1 @@depc_6_bm_58 -1 @@depc_6_xm_56 <= +0
 depc_6_d_78: -1 @@depc_6_xp_55 >= +0
 depc_6_e_79: +1 @@depc_6_bm_58 -1 @@depc_6_xm_56 >= +0
 depc_6_f_80: +1 @@depc_6_re_59 +1 @@depc_6_bp_57 +1 @@depc_6_bm_58 = +1
 depc_6_a_81: +1 x#1#4 -1 @@depc_6_xp_60 +1 @@depc_6_xm_61 = +1
 depc_6_b_82: +1 @@depc_6_bp_62 -1 @@depc_6_xp_60 <= +0
 depc_6_c_83: +1 @@depc_6_bm_63 -1 @@depc_6_xm_61 <= +0
 depc_6_d_84: -1 @@depc_6_xp_60 >= +0
 depc_6_e_85: +1 @@depc_6_bm_63 -1 @@depc_6_xm_61 >= +0
 depc_6_f_86: +1 @@depc_6_re_64 +1 @@depc_6_bp_62 +1 @@depc_6_bm_63 = +1
 depc_6_a_87: +1 @@depc_6_re_59 -1 @@depc_6_re_65 >= +0
 depc_6_b_88: +1 @@depc_6_re_64 -1 @@depc_6_re_65 >= +0
 depc_6_c_89: +1 @@depc_6_re_59 +1 @@depc_6_re_64 -1 @@depc_6_re_65 <= +1
 depc_6_t_l: -1 @@depc_6_re_65 +1 y#1#3#4 >= +0
 depc_7_a_90: +1 x#2#1 -1 @@depc_7_xp_66 +1 @@depc_7_xm_67 = +1
 depc_7_b_91: +1 @@depc_7_bp_68 -1 @@depc_7_xp_66 <= +0
 depc_7_c_92: +1 @@depc_7_bm_69 -1 @@depc_7_xm_67 <= +0
 depc_7_d_93: -1 @@depc_7_xp_66 >= +0
 depc_7_e_94: +1 @@depc_7_bm_69 -1 @@depc_7_xm_67 >= +0
 depc_7_f_95: +1 @@depc_7_re_70 +1 @@depc_7_bp_68 +1 @@depc_7_bm_69 = +1
 depc_7_a_96: +1 x#2#2 -1 @@depc_7_xp_71 +1 @@depc_7_xm_72 = +1
 depc_7_b_97: +1 @@depc_7_bp_73 -1 @@depc_7_xp_71 <= +0
 depc_7_c_98: +1 @@depc_7_bm_74 -1 @@depc_7_xm_72 <= +0
 depc_7_d_99: -1 @@depc_7_xp_71 >= +0
 depc_7_e_100: +1 @@depc_7_bm_74 -1 @@depc_7_xm_72 >= +0
 depc_7_f_101: +1 @@depc_7_re_75 +1 @@depc_7_bp_73 +1 @@depc_7_bm_74 = +1
 depc_7_a_102: +1 @@depc_7_re_70 -1 @@depc_7_re_76 >= +0
 depc_7_b_103: +1 @@depc_7_re_75 -1 @@depc_7_re_76 >= +0
 depc_7_c_104: +1 @@depc_7_re_70 +1 @@depc_7_re_75 -1 @@depc_7_re_76 <= +1
 depc_7_t_l: -1 @@depc_7_re_76 +1 y#2#1#2 >= +0
 depc_8_a_105: +1 x#2#1 -1 @@depc_8_xp_77 +1 @@depc_8_xm_78 = +1
 depc_8_b_106: +1 @@depc_8_bp_79 -1 @@depc_8_xp_77 <= +0
 depc_8_c_107: +1 @@depc_8_bm_80 -1 @@depc_8_xm_78 <= +0
 depc_8_d_108: -1 @@depc_8_xp_77 >= +0
 depc_8_e_109: +1 @@depc_8_bm_80 -1 @@depc_8_xm_78 >= +0
 depc_8_f_110: +1 @@depc_8_re_81 +1 @@depc_8_bp_79 +1 @@depc_8_bm_80 = +1
 depc_8_a_111: +1 x#2#3 -1 @@depc_8_xp_82 +1 @@depc_8_xm_83 = +1
 depc_8_b_112: +1 @@depc_8_bp_84 -1 @@depc_8_xp_82 <= +0
 depc_8_c_113: +1 @@depc_8_bm_85 -1 @@depc_8_xm_83 <= +0
 depc_8_d_114: -1 @@depc_8_xp_82 >= +0
 depc_8_e_115: +1 @@depc_8_bm_85 -1 @@depc_8_xm_83 >= +0
 depc_8_f_116: +1 @@depc_8_re_86 +1 @@depc_8_bp_84 +1 @@depc_8_bm_85 = +1
 depc_8_a_117: +1 @@depc_8_re_81 -1 @@depc_8_re_87 >= +0
 depc_8_b_118: +1 @@depc_8_re_86 -1 @@depc_8_re_87 >= +0
 depc_8_c_119: +1 @@depc_8_re_81 +1 @@depc_8_re_86 -1 @@depc_8_re_87 <= +1
 depc_8_t_l: -1 @@depc_8_re_87 +1 y#2#1#3 >= +0
 depc_9_a_120: +1 x#2#1 -1 @@depc_9_xp_88 +1 @@depc_9_xm_89 = +1
 depc_9_b_121: +1 @@depc_9_bp_90 -1 @@depc_9_xp_88 <= +0
 depc_9_c_122: +1 @@depc_9_bm_91 -1 @@depc_9_xm_89 <= +0
 depc_9_d_123: -1 @@depc_9_xp_88 >= +0
 depc_9_e_124: +1 @@depc_9_bm_91 -1 @@depc_9_xm_89 >= +0
 depc_9_f_125: +1 @@depc_9_re_92 +1 @@depc_9_bp_90 +1 @@depc_9_bm_91 = +1
 depc_9_a_126: +1 x#2#4 -1 @@depc_9_xp_93 +1 @@depc_9_xm_94 = +1
 depc_9_b_127: +1 @@depc_9_bp_95 -1 @@depc_9_xp_93 <= +0
 depc_9_c_128: +1 @@depc_9_bm_96 -1 @@depc_9_xm_94 <= +0
 depc_9_d_129: -1 @@depc_9_xp_93 >= +0
 depc_9_e_130: +1 @@depc_9_bm_96 -1 @@depc_9_xm_94 >= +0
 depc_9_f_131: +1 @@depc_9_re_97 +1 @@depc_9_bp_95 +1 @@depc_9_bm_96 = +1
 depc_9_a_132: +1 @@depc_9_re_92 -1 @@depc_9_re_98 >= +0
 depc_9_b_133: +1 @@depc_9_re_97 -1 @@depc_9_re_98 >= +0
 depc_9_c_134: +1 @@depc_9_re_92 +1 @@depc_9_re_97 -1 @@depc_9_re_98 <= +1
 depc_9_t_l: -1 @@depc_9_re_98 +1 y#2#1#4 >= +0
 depc_10_a_135: +1 x#2#2 -1 @@depc_10_xp_99 +1 @@depc_10_xm_100 = +1
 depc_10_b_136: +1 @@depc_10_bp_101 -1 @@depc_10_xp_99 <= +0
 depc_10_c_137: +1 @@depc_10_bm_102 -1 @@depc_10_xm_100 <= +0
 depc_10_d_138: -1 @@depc_10_xp_99 >= +0
 depc_10_e_139: +1 @@depc_10_bm_102 -1 @@depc_10_xm_100 >= +0
 depc_10_f_140: +1 @@depc_10_re_103 +1 @@depc_10_bp_101 +1 @@depc_10_bm_102 = +1
 depc_10_a_141: +1 x#2#3 -1 @@depc_10_xp_104 +1 @@depc_10_xm_105 = +1
 depc_10_b_142: +1 @@depc_10_bp_106 -1 @@depc_10_xp_104 <= +0
 depc_10_c_143: +1 @@depc_10_bm_107 -1 @@depc_10_xm_105 <= +0
 depc_10_d_144: -1 @@depc_10_xp_104 >= +0
 depc_10_e_145: +1 @@depc_10_bm_107 -1 @@depc_10_xm_105 >= +0
 depc_10_f_146: +1 @@depc_10_re_108 +1 @@depc_10_bp_106 +1 @@depc_10_bm_107 = +1
 depc_10_a_147: +1 @@depc_10_re_103 -1 @@depc_10_re_109 >= +0
 depc_10_b_148: +1 @@depc_10_re_108 -1 @@depc_10_re_109 >= +0
 depc_10_c_149: +1 @@depc_10_re_103 +1 @@depc_10_re_108 -1 @@depc_10_re_109 <= +1
 depc_10_t_l: -1 @@depc_10_re_109 +1 y#2#2#3 >= +0
 depc_11_a_150: +1 x#2#2 -1 @@depc_11_xp_110 +1 @@depc_11_xm_111 = +1
 depc_11_b_151: +1 @@depc_11_bp_112 -1 @@depc_11_xp_110 <= +0
 depc_11_c_152: +1 @@depc_11_bm_113 -1 @@depc_11_xm_111 <= +0
 depc_11_d_153: -1 @@depc_11_xp_110 >= +0
 depc_11_e_154: +1 @@depc_11_bm_113 -1 @@depc_11_xm_111 >= +0
 depc_11_f_155: +1 @@depc_11_re_114 +1 @@depc_11_bp_112 +1 @@depc_11_bm_113 = +1
 depc_11_a_156: +1 x#2#4 -1 @@depc_11_xp_115 +1 @@depc_11_xm_116 = +1
 depc_11_b_157: +1 @@depc_11_bp_117 -1 @@depc_11_xp_115 <= +0
 depc_11_c_158: +1 @@depc_11_bm_118 -1 @@depc_11_xm_116 <= +0
 depc_11_d_159: -1 @@depc_11_xp_115 >= +0
 depc_11_e_160: +1 @@depc_11_bm_118 -1 @@depc_11_xm_116 >= +0
 depc_11_f_161: +1 @@depc_11_re_119 +1 @@depc_11_bp_117 +1 @@depc_11_bm_118 = +1
 depc_11_a_162: +1 @@depc_11_re_114 -1 @@depc_11_re_120 >= +0
 depc_11_b_163: +1 @@depc_11_re_119 -1 @@depc_11_re_120 >= +0
 depc_11_c_164: +1 @@depc_11_re_114 +1 @@depc_11_re_119 -1 @@depc_11_re_120 <= +1
 depc_11_t_l: -1 @@depc_11_re_120 +1 y#2#2#4 >= +0
 depc_12_a_165: +1 x#2#3 -1 @@depc_12_xp_121 +1 @@depc_12_xm_122 = +1
 depc_12_b_166: +1 @@depc_12_bp_123 -1 @@depc_12_xp_121 <= +0
 depc_12_c_167: +1 @@depc_12_bm_124 -1 @@depc_12_xm_122 <= +0
 depc_12_d_168: -1 @@depc_12_xp_121 >= +0
 depc_12_e_169: +1 @@depc_12_bm_124 -1 @@depc_12_xm_122 >= +0
 depc_12_f_170: +1 @@depc_12_re_125 +1 @@depc_12_bp_123 +1 @@depc_12_bm_124 = +1
 depc_12_a_171: +1 x#2#4 -1 @@depc_12_xp_126 +1 @@depc_12_xm_127 = +1
 depc_12_b_172: +1 @@depc_12_bp_128 -1 @@depc_12_xp_126 <= +0
 depc_12_c_173: +1 @@depc_12_bm_129 -1 @@depc_12_xm_127 <= +0
 depc_12_d_174: -1 @@depc_12_xp_126 >= +0
 depc_12_e_175: +1 @@depc_12_bm_129 -1 @@depc_12_xm_127 >= +0
 depc_12_f_176: +1 @@depc_12_re_130 +1 @@depc_12_bp_128 +1 @@depc_12_bm_129 = +1
 depc_12_a_177: +1 @@depc_12_re_125 -1 @@depc_12_re_131 >= +0
 depc_12_b_178: +1 @@depc_12_re_130 -1 @@depc_12_re_131 >= +0
 depc_12_c_179: +1 @@depc_12_re_125 +1 @@depc_12_re_130 -1 @@depc_12_re_131 <= +1
 depc_12_t_l: -1 @@depc_12_re_131 +1 y#2#3#4 >= +0
 depc_13_a_180: +1 x#3#1 -1 @@depc_13_xp_132 +1 @@depc_13_xm_133 = +1
 depc_13_b_181: +1 @@depc_13_bp_134 -1 @@depc_13_xp_132 <= +0
 depc_13_c_182: +1 @@depc_13_bm_135 -1 @@depc_13_xm_133 <= +0
 depc_13_d_183: -1 @@depc_13_xp_132 >= +0
 depc_13_e_184: +1 @@depc_13_bm_135 -1 @@depc_13_xm_133 >= +0
 depc_13_f_185: +1 @@depc_13_re_136 +1 @@depc_13_bp_134 +1 @@depc_13_bm_135 = +1
 depc_13_a_186: +1 x#3#2 -1 @@depc_13_xp_137 +1 @@depc_13_xm_138 = +1
 depc_13_b_187: +1 @@depc_13_bp_139 -1 @@depc_13_xp_137 <= +0
 depc_13_c_188: +1 @@depc_13_bm_140 -1 @@depc_13_xm_138 <= +0
 depc_13_d_189: -1 @@depc_13_xp_137 >= +0
 depc_13_e_190: +1 @@depc_13_bm_140 -1 @@depc_13_xm_138 >= +0
 depc_13_f_191: +1 @@depc_13_re_141 +1 @@depc_13_bp_139 +1 @@depc_13_bm_140 = +1
 depc_13_a_192: +1 @@depc_13_re_136 -1 @@depc_13_re_142 >= +0
 depc_13_b_193: +1 @@depc_13_re_141 -1 @@depc_13_re_142 >= +0
 depc_13_c_194: +1 @@depc_13_re_136 +1 @@depc_13_re_141 -1 @@depc_13_re_142 <= +1
 depc_13_t_l: -1 @@depc_13_re_142 +1 y#3#1#2 >= +0
 depc_14_a_195: +1 x#3#1 -1 @@depc_14_xp_143 +1 @@depc_14_xm_144 = +1
 depc_14_b_196: +1 @@depc_14_bp_145 -1 @@depc_14_xp_143 <= +0
 depc_14_c_197: +1 @@depc_14_bm_146 -1 @@depc_14_xm_144 <= +0
 depc_14_d_198: -1 @@depc_14_xp_143 >= +0
 depc_14_e_199: +1 @@depc_14_bm_146 -1 @@depc_14_xm_144 >= +0
 depc_14_f_200: +1 @@depc_14_re_147 +1 @@depc_14_bp_145 +1 @@depc_14_bm_146 = +1
 depc_14_a_201: +1 x#3#3 -1 @@depc_14_xp_148 +1 @@depc_14_xm_149 = +1
 depc_14_b_202: +1 @@depc_14_bp_150 -1 @@depc_14_xp_148 <= +0
 depc_14_c_203: +1 @@depc_14_bm_151 -1 @@depc_14_xm_149 <= +0
 depc_14_d_204: -1 @@depc_14_xp_148 >= +0
 depc_14_e_205: +1 @@depc_14_bm_151 -1 @@depc_14_xm_149 >= +0
 depc_14_f_206: +1 @@depc_14_re_152 +1 @@depc_14_bp_150 +1 @@depc_14_bm_151 = +1
 depc_14_a_207: +1 @@depc_14_re_147 -1 @@depc_14_re_153 >= +0
 depc_14_b_208: +1 @@depc_14_re_152 -1 @@depc_14_re_153 >= +0
 depc_14_c_209: +1 @@depc_14_re_147 +1 @@depc_14_re_152 -1 @@depc_14_re_153 <= +1
 depc_14_t_l: -1 @@depc_14_re_153 +1 y#3#1#3 >= +0
 depc_15_a_210: +1 x#3#1 -1 @@depc_15_xp_154 +1 @@depc_15_xm_155 = +1
 depc_15_b_211: +1 @@depc_15_bp_156 -1 @@depc_15_xp_154 <= +0
 depc_15_c_212: +1 @@depc_15_bm_157 -1 @@depc_15_xm_155 <= +0
 depc_15_d_213: -1 @@depc_15_xp_154 >= +0
 depc_15_e_214: +1 @@depc_15_bm_157 -1 @@depc_15_xm_155 >= +0
 depc_15_f_215: +1 @@depc_15_re_158 +1 @@depc_15_bp_156 +1 @@depc_15_bm_157 = +1
 depc_15_a_216: +1 x#3#4 -1 @@depc_15_xp_159 +1 @@depc_15_xm_160 = +1
 depc_15_b_217: +1 @@depc_15_bp_161 -1 @@depc_15_xp_159 <= +0
 depc_15_c_218: +1 @@depc_15_bm_162 -1 @@depc_15_xm_160 <= +0
 depc_15_d_219: -1 @@depc_15_xp_159 >= +0
 depc_15_e_220: +1 @@depc_15_bm_162 -1 @@depc_15_xm_160 >= +0
 depc_15_f_221: +1 @@depc_15_re_163 +1 @@depc_15_bp_161 +1 @@depc_15_bm_162 = +1
 depc_15_a_222: +1 @@depc_15_re_158 -1 @@depc_15_re_164 >= +0
 depc_15_b_223: +1 @@depc_15_re_163 -1 @@depc_15_re_164 >= +0
 depc_15_c_224: +1 @@depc_15_re_158 +1 @@depc_15_re_163 -1 @@depc_15_re_164 <= +1
 depc_15_t_l: -1 @@depc_15_re_164 +1 y#3#1#4 >= +0
 depc_16_a_225: +1 x#3#2 -1 @@depc_16_xp_165 +1 @@depc_16_xm_166 = +1
 depc_16_b_226: +1 @@depc_16_bp_167 -1 @@depc_16_xp_165 <= +0
 depc_16_c_227: +1 @@depc_16_bm_168 -1 @@depc_16_xm_166 <= +0
 depc_16_d_228: -1 @@depc_16_xp_165 >= +0
 depc_16_e_229: +1 @@depc_16_bm_168 -1 @@depc_16_xm_166 >= +0
 depc_16_f_230: +1 @@depc_16_re_169 +1 @@depc_16_bp_167 +1 @@depc_16_bm_168 = +1
 depc_16_a_231: +1 x#3#3 -1 @@depc_16_xp_170 +1 @@depc_16_xm_171 = +1
 depc_16_b_232: +1 @@depc_16_bp_172 -1 @@depc_16_xp_170 <= +0
 depc_16_c_233: +1 @@depc_16_bm_173 -1 @@depc_16_xm_171 <= +0
 depc_16_d_234: -1 @@depc_16_xp_170 >= +0
 depc_16_e_235: +1 @@depc_16_bm_173 -1 @@depc_16_xm_171 >= +0
 depc_16_f_236: +1 @@depc_16_re_174 +1 @@depc_16_bp_172 +1 @@depc_16_bm_173 = +1
 depc_16_a_237: +1 @@depc_16_re_169 -1 @@depc_16_re_175 >= +0
 depc_16_b_238: +1 @@depc_16_re_174 -1 @@depc_16_re_175 >= +0
 depc_16_c_239: +1 @@depc_16_re_169 +1 @@depc_16_re_174 -1 @@depc_16_re_175 <= +1
 depc_16_t_l: -1 @@depc_16_re_175 +1 y#3#2#3 >= +0
 depc_17_a_240: +1 x#3#2 -1 @@depc_17_xp_176 +1 @@depc_17_xm_177 = +1
 depc_17_b_241: +1 @@depc_17_bp_178 -1 @@depc_17_xp_176 <= +0
 depc_17_c_242: +1 @@depc_17_bm_179 -1 @@depc_17_xm_177 <= +0
 depc_17_d_243: -1 @@depc_17_xp_176 >= +0
 depc_17_e_244: +1 @@depc_17_bm_179 -1 @@depc_17_xm_177 >= +0
 depc_17_f_245: +1 @@depc_17_re_180 +1 @@depc_17_bp_178 +1 @@depc_17_bm_179 = +1
 depc_17_a_246: +1 x#3#4 -1 @@depc_17_xp_181 +1 @@depc_17_xm_182 = +1
 depc_17_b_247: +1 @@depc_17_bp_183 -1 @@depc_17_xp_181 <= +0
 depc_17_c_248: +1 @@depc_17_bm_184 -1 @@depc_17_xm_182 <= +0
 depc_17_d_249: -1 @@depc_17_xp_181 >= +0
 depc_17_e_250: +1 @@depc_17_bm_184 -1 @@depc_17_xm_182 >= +0
 depc_17_f_251: +1 @@depc_17_re_185 +1 @@depc_17_bp_183 +1 @@depc_17_bm_184 = +1
 depc_17_a_252: +1 @@depc_17_re_180 -1 @@depc_17_re_186 >= +0
 depc_17_b_253: +1 @@depc_17_re_185 -1 @@depc_17_re_186 >= +0
 depc_17_c_254: +1 @@depc_17_re_180 +1 @@depc_17_re_185 -1 @@depc_17_re_186 <= +1
 depc_17_t_l: -1 @@depc_17_re_186 +1 y#3#2#4 >= +0
 depc_18_a_255: +1 x#3#3 -1 @@depc_18_xp_187 +1 @@depc_18_xm_188 = +1
 depc_18_b_256: +1 @@depc_18_bp_189 -1 @@depc_18_xp_187 <= +0
 depc_18_c_257: +1 @@depc_18_bm_190 -1 @@depc_18_xm_188 <= +0
 depc_18_d_258: -1 @@depc_18_xp_187 >= +0
 depc_18_e_259: +1 @@depc_18_bm_190 -1 @@depc_18_xm_188 >= +0
 depc_18_f_260: +1 @@depc_18_re_191 +1 @@depc_18_bp_189 +1 @@depc_18_bm_190 = +1
 depc_18_a_261: +1 x#3#4 -1 @@depc_18_xp_192 +1 @@depc_18_xm_193 = +1
 depc_18_b_262: +1 @@depc_18_bp_194 -1 @@depc_18_xp_192 <= +0
 depc_18_c_263: +1 @@depc_18_bm_195 -1 @@depc_18_xm_193 <= +0
 depc_18_d_264: -1 @@depc_18_xp_192 >= +0
 depc_18_e_265: +1 @@depc_18_bm_195 -1 @@depc_18_xm_193 >= +0
 depc_18_f_266: +1 @@depc_18_re_196 +1 @@depc_18_bp_194 +1 @@depc_18_bm_195 = +1
 depc_18_a_267: +1 @@depc_18_re_191 -1 @@depc_18_re_197 >= +0
 depc_18_b_268: +1 @@depc_18_re_196 -1 @@depc_18_re_197 >= +0
 depc_18_c_269: +1 @@depc_18_re_191 +1 @@depc_18_re_196 -1 @@depc_18_re_197 <= +1
 depc_18_t_l: -1 @@depc_18_re_197 +1 y#3#3#4 >= +0
Bounds
 0 <= x#1#1 <= 1
 0 <= x#1#2 <= 1
 0 <= x#1#3 <= 1
 0 <= x#1#4 <= 1
 0 <= x#2#1 <= 1
 0 <= x#2#2 <= 1
 0 <= x#2#3 <= 1
 0 <= x#2#4 <= 1
 0 <= x#3#1 <= 1
 0 <= x#3#2 <= 1
 0 <= x#3#3 <= 1
 0 <= x#3#4 <= 1
 0 <= y#1#1#1 <= 1
 0 <= y#1#1#2 <= 1
 0 <= y#1#1#3 <= 1
 0 <= y#1#1#4 <= 1
 0 <= y#1#2#1 <= 1
 0 <= y#1#2#2 <= 1
 0 <= y#1#2#3 <= 1
 0 <= y#1#2#4 <= 1
 0 <= y#1#3#1 <= 1
 0 <= y#1#3#2 <= 1
 0 <= y#1#3#3 <= 1
 0 <= y#1#3#4 <= 1
 0 <= y#1#4#1 <= 1
 0 <= y#1#4#2 <= 1
 0 <= y#1#4#3 <= 1
 0 <= y#1#4#4 <= 1
 0 <= y#2#1#1 <= 1
 0 <= y#2#1#2 <= 1
 0 <= y#2#1#3 <= 1
 0 <= y#2#1#4 <= 1
 0 <= y#2#2#1 <= 1
 0 <= y#2#2#2 <= 1
 0 <= y#2#2#3 <= 1
 0 <= y#2#2#4 <= 1
 0 <= y#2#3#1 <= 1
 0 <= y#2#3#2 <= 1
 0 <= y#2#3#3 <= 1
 0 <= y#2#3#4 <= 1
 0 <= y#2#4#1 <= 1
 0 <= y#2#4#2 <= 1
 0 <= y#2#4#3 <= 1
 0 <= y#2#4#4 <= 1
 0 <= y#3#1#1 <= 1
 0 <= y#3#1#2 <= 1
 0 <= y#3#1#3 <= 1
 0 <= y#3#1#4 <= 1
 0 <= y#3#2#1 <= 1
 0 <= y#3#2#2 <= 1
 0 <= y#3#2#3 <= 1
 0 <= y#3#2#4 <= 1
 0 <= y#3#3#1 <= 1
 0 <= y#3#3#2 <= 1
 0 <= y#3#3#3 <= 1
 0 <= y#3#3#4 <= 1
 0 <= y#3#4#1 <= 1
 0 <= y#3#4#2 <= 1
 0 <= y#3#4#3 <= 1
 0 <= y#3#4#4 <= 1
 0 <= @@depc_1_xp_0 <= 0
 0 <= @@depc_1_xm_1 <= 1
 0 <= @@depc_1_bp_2 <= 1
 0 <= @@depc_1_bm_3 <= 1
 0 <= @@depc_1_re_4 <= 1
 0 <= @@depc_1_xp_5 <= 0
 0 <= @@depc_1_xm_6 <= 1
 0 <= @@depc_1_bp_7 <= 1
 0 <= @@depc_1_bm_8 <= 1
 0 <= @@depc_1_re_9 <= 1
 0 <= @@depc_1_re_10 <= 1
 0 <= @@depc_2_xp_11 <= 0
 0 <= @@depc_2_xm_12 <= 1
 0 <= @@depc_2_bp_13 <= 1
 0 <= @@depc_2_bm_14 <= 1
 0 <= @@depc_2_re_15 <= 1
 0 <= @@depc_2_xp_16 <= 0
 0 <= @@depc_2_xm_17 <= 1
 0 <= @@depc_2_bp_18 <= 1
 0 <= @@depc_2_bm_19 <= 1
 0 <= @@depc_2_re_20 <= 1
 0 <= @@depc_2_re_21 <= 1
 0 <= @@depc_3_xp_22 <= 0
 0 <= @@depc_3_xm_23 <= 1
 0 <= @@depc_3_bp_24 <= 1
 0 <= @@depc_3_bm_25 <= 1
 0 <= @@depc_3_re_26 <= 1
 0 <= @@depc_3_xp_27 <= 0
 0 <= @@depc_3_xm_28 <= 1
 0 <= @@depc_3_bp_29 <= 1
 0 <= @@depc_3_bm_30 <= 1
 0 <= @@depc_3_re_31 <= 1
 0 <= @@depc_3_re_32 <= 1
 0 <= @@depc_4_xp_33 <= 0
 0 <= @@depc_4_xm_34 <= 1
 0 <= @@depc_4_bp_35 <= 1
 0 <= @@depc_4_bm_36 <= 1
 0 <= @@depc_4_re_37 <= 1
 0 <= @@depc_4_xp_38 <= 0
 0 <= @@depc_4_xm_39 <= 1
 0 <= @@depc_4_bp_40 <= 1
 0 <= @@depc_4_bm_41 <= 1
 0 <= @@depc_4_re_42 <= 1
 0 <= @@depc_4_re_43 <= 1
 0 <= @@depc_5_xp_44 <= 0
 0 <= @@depc_5_xm_45 <= 1
 0 <= @@depc_5_bp_46 <= 1
 0 <= @@depc_5_bm_47 <= 1
 0 <= @@depc_5_re_48 <= 1
 0 <= @@depc_5_xp_49 <= 0
 0 <= @@depc_5_xm_50 <= 1
 0 <= @@depc_5_bp_51 <= 1
 0 <= @@depc_5_bm_52 <= 1
 0 <= @@depc_5_re_53 <= 1
 0 <= @@depc_5_re_54 <= 1
 0 <= @@depc_6_xp_55 <= 0
 0 <= @@depc_6_xm_56 <= 1
 0 <= @@depc_6_bp_57 <= 1
 0 <= @@depc_6_bm_58 <= 1
 0 <= @@depc_6_re_59 <= 1
 0 <= @@depc_6_xp_60 <= 0
 0 <= @@depc_6_xm_61 <= 1
 0 <= @@depc_6_bp_62 <= 1
 0 <= @@depc_6_bm_63 <= 1
 0 <= @@depc_6_re_64 <= 1
 0 <= @@depc_6_re_65 <= 1
 0 <= @@depc_7_xp_66 <= 0
 0 <= @@depc_7_xm_67 <= 1
 0 <= @@depc_7_bp_68 <= 1
 0 <= @@depc_7_bm_69 <= 1
 0 <= @@depc_7_re_70 <= 1
 0 <= @@depc_7_xp_71 <= 0
 0 <= @@depc_7_xm_72 <= 1
 0 <= @@depc_7_bp_73 <= 1
 0 <= @@depc_7_bm_74 <= 1
 0 <= @@depc_7_re_75 <= 1
 0 <= @@depc_7_re_76 <= 1
 0 <= @@depc_8_xp_77 <= 0
 0 <= @@depc_8_xm_78 <= 1
 0 <= @@depc_8_bp_79 <= 1
 0 <= @@depc_8_bm_80 <= 1
 0 <= @@depc_8_re_81 <= 1
 0 <= @@depc_8_xp_82 <= 0
 0 <= @@depc_8_xm_83 <= 1
 0 <= @@depc_8_bp_84 <= 1
 0 <= @@depc_8_bm_85 <= 1
 0 <= @@depc_8_re_86 <= 1
 0 <= @@depc_8_re_87 <= 1
 0 <= @@depc_9_xp_88 <= 0
 0 <= @@depc_9_xm_89 <= 1
 0 <= @@depc_9_bp_90 <= 1
 0 <= @@depc_9_bm_91 <= 1
 0 <= @@depc_9_re_92 <= 1
 0 <= @@depc_9_xp_93 <= 0
 0 <= @@depc_9_xm_94 <= 1
 0 <= @@depc_9_bp_95 <= 1
 0 <= @@depc_9_bm_96 <= 1
 0 <= @@depc_9_re_97 <= 1
 0 <= @@depc_9_re_98 <= 1
 0 <= @@depc_10_xp_99 <= 0
 0 <= @@depc_10_xm_100 <= 1
 0 <= @@depc_10_bp_101 <= 1
 0 <= @@depc_10_bm_102 <= 1
 0 <= @@depc_10_re_103 <= 1
 0 <= @@depc_10_xp_104 <= 0
 0 <= @@depc_10_xm_105 <= 1
 0 <= @@depc_10_bp_106 <= 1
 0 <= @@depc_10_bm_107 <= 1
 0 <= @@depc_10_re_108 <= 1
 0 <= @@depc_10_re_109 <= 1
 0 <= @@depc_11_xp_110 <= 0
 0 <= @@depc_11_xm_111 <= 1
 0 <= @@depc_11_bp_112 <= 1
 0 <= @@depc_11_bm_113 <= 1
 0 <= @@depc_11_re_114 <= 1
 0 <= @@depc_11_xp_115 <= 0
 0 <= @@depc_11_xm_116 <= 1
 0 <= @@depc_11_bp_117 <= 1
 0 <= @@depc_11_bm_118 <= 1
 0 <= @@depc_11_re_119 <= 1
 0 <= @@depc_11_re_120 <= 1
 0 <= @@depc_12_xp_121 <= 0
 0 <= @@depc_12_xm_122 <= 1
 0 <= @@depc_12_bp_123 <= 1
 0 <= @@depc_12_bm_124 <= 1
 0 <= @@depc_12_re_125 <= 1
 0 <= @@depc_12_xp_126 <= 0
 0 <= @@depc_12_xm_127 <= 1
 0 <= @@depc_12_bp_128 <= 1
 0 <= @@depc_12_bm_129 <= 1
 0 <= @@depc_12_re_130 <= 1
 0 <= @@depc_12_re_131 <= 1
 0 <= @@depc_13_xp_132 <= 0
 0 <= @@depc_13_xm_133 <= 1
 0 <= @@depc_13_bp_134 <= 1
 0 <= @@depc_13_bm_135 <= 1
 0 <= @@depc_13_re_136 <= 1
 0 <= @@depc_13_xp_137 <= 0
 0 <= @@depc_13_xm_138 <= 1
 0 <= @@depc_13_bp_139 <= 1
 0 <= @@depc_13_bm_140 <= 1
 0 <= @@depc_13_re_141 <= 1
 0 <= @@depc_13_re_142 <= 1
 0 <= @@depc_14_xp_143 <= 0
 0 <= @@depc_14_xm_144 <= 1
 0 <= @@depc_14_bp_145 <= 1
 0 <= @@depc_14_bm_146 <= 1
 0 <= @@depc_14_re_147 <= 1
 0 <= @@depc_14_xp_148 <= 0
 0 <= @@depc_14_xm_149 <= 1
 0 <= @@depc_14_bp_150 <= 1
 0 <= @@depc_14_bm_151 <= 1
 0 <= @@depc_14_re_152 <= 1
 0 <= @@depc_14_re_153 <= 1
 0 <= @@depc_15_xp_154 <= 0
 0 <= @@depc_15_xm_155 <= 1
 0 <= @@depc_15_bp_156 <= 1
 0 <= @@depc_15_bm_157 <= 1
 0 <= @@depc_15_re_158 <= 1
 0 <= @@depc_15_xp_159 <= 0
 0 <= @@depc_15_xm_160 <= 1
 0 <= @@depc_15_bp_161 <= 1
 0 <= @@depc_15_bm_162 <= 1
 0 <= @@depc_15_re_163 <= 1
 0 <= @@depc_15_re_164 <= 1
 0 <= @@depc_16_xp_165 <= 0
 0 <= @@depc_16_xm_166 <= 1
 0 <= @@depc_16_bp_167 <= 1
 0 <= @@depc_16_bm_168 <= 1
 0 <= @@depc_16_re_169 <= 1
 0 <= @@depc_16_xp_170 <= 0
 0 <= @@depc_16_xm_171 <= 1
 0 <= @@depc_16_bp_172 <= 1
 0 <= @@depc_16_bm_173 <= 1
 0 <= @@depc_16_re_174 <= 1
 0 <= @@depc_16_re_175 <= 1
 0 <= @@depc_17_xp_176 <= 0
 0 <= @@depc_17_xm_177 <= 1
 0 <= @@depc_17_bp_178 <= 1
 0 <= @@depc_17_bm_179 <= 1
 0 <= @@depc_17_re_180 <= 1
 0 <= @@depc_17_xp_181 <= 0
 0 <= @@depc_17_xm_182 <= 1
 0 <= @@depc_17_bp_183 <= 1
 0 <= @@depc_17_bm_184 <= 1
 0 <= @@depc_17_re_185 <= 1
 0 <= @@depc_17_re_186 <= 1
 0 <= @@depc_18_xp_187 <= 0
 0 <= @@depc_18_xm_188 <= 1
 0 <= @@depc_18_bp_189 <= 1
 0 <= @@depc_18_bm_190 <= 1
 0 <= @@depc_18_re_191 <= 1
 0 <= @@depc_18_xp_192 <= 0
 0 <= @@depc_18_xm_193 <= 1
 0 <= @@depc_18_bp_194 <= 1
 0 <= @@depc_18_bm_195 <= 1
 0 <= @@depc_18_re_196 <= 1
 0 <= @@depc_18_re_197 <= 1
Binaries
 x#1#1 x#1#2 x#1#3 x#1#4 x#2#1 x#2#2 x#2#3 x#2#4 x#3#1 x#3#2 x#3#3 x#3#4 y#1#1#1 y#1#1#2 y#1#1#3 y#1#1#4
 y#1#2#1 y#1#2#2 y#1#2#3 y#1#2#4 y#1#3#1 y#1#3#2 y#1#3#3 y#1#3#4 y#1#4#1 y#1#4#2 y#1#4#3 y#1#4#4 y#2#1#1
 y#2#1#2 y#2#1#3 y#2#1#4 y#2#2#1 y#2#2#2 y#2#2#3 y#2#2#4 y#2#3#1 y#2#3#2 y#2#3#3 y#2#3#4 y#2#4#1 y#2#4#2
 y#2#4#3 y#2#4#4 y#3#1#1 y#3#1#2 y#3#1#3 y#3#1#4 y#3#2#1 y#3#2#2 y#3#2#3 y#3#2#4 y#3#3#1 y#3#3#2 y#3#3#3
 y#3#3#4 y#3#4#1 y#3#4#2 y#3#4#3 y#3#4#4 @@depc_1_xp_0 @@depc_1_xm_1 @@depc_1_bp_2 @@depc_1_bm_3 @@depc_1_re_4
 @@depc_1_xp_5 @@depc_1_xm_6 @@depc_1_bp_7 @@depc_1_bm_8 @@depc_1_re_9 @@depc_1_re_10 @@depc_2_xp_11 @@depc_2_xm_12
 @@depc_2_bp_13 @@depc_2_bm_14 @@depc_2_re_15 @@depc_2_xp_16 @@depc_2_xm_17 @@depc_2_bp_18 @@depc_2_bm_19
 @@depc_2_re_20 @@depc_2_re_21 @@depc_3_xp_22 @@depc_3_xm_23 @@depc_3_bp_24 @@depc_3_bm_25 @@depc_3_re_26
 @@depc_3_xp_27 @@depc_3_xm_28 @@depc_3_bp_29 @@depc_3_bm_30 @@depc_3_re_31 @@depc_3_re_32 @@depc_4_xp_33
 @@depc_4_xm_34 @@depc_4_bp_35 @@depc_4_bm_36 @@depc_4_re_37 @@depc_4_xp_38 @@depc_4_xm_39 @@depc_4_bp_40
 @@depc_4_bm_41 @@depc_4_re_42 @@depc_4_re_43 @@depc_5_xp_44 @@depc_5_xm_45 @@depc_5_bp_46 @@depc_5_bm_47
 @@depc_5_re_48 @@depc_5_xp_49 @@depc_5_xm_50 @@depc_5_bp_51 @@depc_5_bm_52 @@depc_5_re_53 @@depc_5_re_54
 @@depc_6_xp_55 @@depc_6_xm_56 @@depc_6_bp_57 @@depc_6_bm_58 @@depc_6_re_59 @@depc_6_xp_60 @@depc_6_xm_61
 @@depc_6_bp_62 @@depc_6_bm_63 @@depc_6_re_64 @@depc_6_re_65 @@depc_7_xp_66 @@depc_7_xm_67 @@depc_7_bp_68
 @@depc_7_bm_69 @@depc_7_re_70 @@depc_7_xp_71 @@depc_7_xm_72 @@depc_7_bp_73 @@depc_7_bm_74 @@depc_7_re_75
 @@depc_7_re_76 @@depc_8_xp_77 @@depc_8_xm_78 @@depc_8_bp_79 @@depc_8_bm_80 @@depc_8_re_81 @@depc_8_xp_82
 @@depc_8_xm_83 @@depc_8_bp_84 @@depc_8_bm_85 @@depc_8_re_86 @@depc_8_re_87 @@depc_9_xp_88 @@depc_9_xm_89
 @@depc_9_bp_90 @@depc_9_bm_91 @@depc_9_re_92 @@depc_9_xp_93 @@depc_9_xm_94 @@depc_9_bp_95 @@depc_9_bm_96
 @@depc_9_re_97 @@depc_9_re_98 @@depc_10_xp_99 @@depc_10_xm_100 @@depc_10_bp_101 @@depc_10_bm_102 @@depc_10_re_103
 @@depc_10_xp_104 @@depc_10_xm_105 @@depc_10_bp_106 @@depc_10_bm_107 @@depc_10_re_108 @@depc_10_re_109
 @@depc_11_xp_110 @@depc_11_xm_111 @@depc_11_bp_112 @@depc_11_bm_113 @@depc_11_re_114 @@depc_11_xp_115
 @@depc_11_xm_116 @@depc_11_bp_117 @@depc_11_bm_118 @@depc_11_re_119 @@depc_11_re_120 @@depc_12_xp_121
 @@depc_12_xm_122 @@depc_12_bp_123 @@depc_12_bm_124 @@depc_12_re_125 @@depc_12_xp_126 @@depc_12_xm_127
 @@depc_12_bp_128 @@depc_12_bm_129 @@depc_12_re_130 @@depc_12_re_131 @@depc_13_xp_132 @@depc_13_xm_133
 @@depc_13_bp_134 @@depc_13_bm_135 @@depc_13_re_136 @@depc_13_xp_137 @@depc_13_xm_138 @@depc_13_bp_139
 @@depc_13_bm_140 @@depc_13_re_141 @@depc_13_re_142 @@depc_14_xp_143 @@depc_14_xm_144 @@depc_14_bp_145
 @@depc_14_bm_146 @@depc_14_re_147 @@depc_14_xp_148 @@depc_14_xm_149 @@depc_14_bp_150 @@depc_14_bm_151
 @@depc_14_re_152 @@depc_14_re_153 @@depc_15_xp_154 @@depc_15_xm_155 @@depc_15_bp_156 @@depc_15_bm_157
 @@depc_15_re_158 @@depc_15_xp_159 @@depc_15_xm_160 @@depc_15_bp_161 @@depc_15_bm_162 @@depc_15_re_163
 @@depc_15_re_164 @@depc_16_xp_165 @@depc_16_xm_166 @@depc_16_bp_167 @@depc_16_bm_168 @@depc_16_re_169
 @@depc_16_xp_170 @@depc_16_xm_171 @@depc_16_bp_172 @@depc_16_bm_173 @@depc_16_re_174 @@depc_16_re_175
 @@depc_17_xp_176 @@depc_17_xm_177 @@depc_17_bp_178 @@depc_17_bm_179 @@depc_17_re_180 @@depc_17_xp_181
 @@depc_17_xm_182 @@depc_17_bp_183 @@depc_17_bm_184 @@depc_17_re_185 @@depc_17_re_186 @@depc_18_xp_187
 @@depc_18_xm_188 @@depc_18_bp_189 @@depc_18_bm_190 @@depc_18_re_191 @@depc_18_xp_192 @@depc_18_xm_193
 @@depc_18_bp_194 @@depc_18_bm_195 @@depc_18_re_196 @@depc_18_re_197
End
