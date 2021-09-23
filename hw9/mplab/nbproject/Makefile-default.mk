#
# Generated Makefile - do not edit!
#
# Edit the Makefile in the project folder instead (../Makefile). Each target
# has a -pre and a -post target defined where you can add customized code.
#
# This makefile implements configuration specific macros and targets.


# Include project Makefile
ifeq "${IGNORE_LOCAL}" "TRUE"
# do not include local makefile. User is passing all local related variables already
else
include Makefile
# Include makefile containing local settings
ifeq "$(wildcard nbproject/Makefile-local-default.mk)" "nbproject/Makefile-local-default.mk"
include nbproject/Makefile-local-default.mk
endif
endif

# Environment
MKDIR=mkdir -p
RM=rm -f 
MV=mv 
CP=cp 

# Macros
CND_CONF=default
ifeq ($(TYPE_IMAGE), DEBUG_RUN)
IMAGE_TYPE=debug
OUTPUT_SUFFIX=cof
DEBUGGABLE_SUFFIX=cof
FINAL_IMAGE=dist/${CND_CONF}/${IMAGE_TYPE}/mplab.${IMAGE_TYPE}.${OUTPUT_SUFFIX}
else
IMAGE_TYPE=production
OUTPUT_SUFFIX=hex
DEBUGGABLE_SUFFIX=cof
FINAL_IMAGE=dist/${CND_CONF}/${IMAGE_TYPE}/mplab.${IMAGE_TYPE}.${OUTPUT_SUFFIX}
endif

ifeq ($(COMPARE_BUILD), true)
COMPARISON_BUILD=
else
COMPARISON_BUILD=
endif

ifdef SUB_IMAGE_ADDRESS

else
SUB_IMAGE_ADDRESS_COMMAND=
endif

# Object Directory
OBJECTDIR=build/${CND_CONF}/${IMAGE_TYPE}

# Distribution Directory
DISTDIR=dist/${CND_CONF}/${IMAGE_TYPE}

# Source Files Quoted if spaced
SOURCEFILES_QUOTED_IF_SPACED=uni-dds-hw9.asm

# Object Files Quoted if spaced
OBJECTFILES_QUOTED_IF_SPACED=${OBJECTDIR}/uni-dds-hw9.o
POSSIBLE_DEPFILES=${OBJECTDIR}/uni-dds-hw9.o.d

# Object Files
OBJECTFILES=${OBJECTDIR}/uni-dds-hw9.o

# Source Files
SOURCEFILES=uni-dds-hw9.asm



CFLAGS=
ASFLAGS=
LDLIBSOPTIONS=

############# Tool locations ##########################################
# If you copy a project from one host to another, the path where the  #
# compiler is installed may be different.                             #
# If you open this project with MPLAB X in the new host, this         #
# makefile will be regenerated and the paths will be corrected.       #
#######################################################################
# fixDeps replaces a bunch of sed/cat/printf statements that slow down the build
FIXDEPS=fixDeps

.build-conf:  ${BUILD_SUBPROJECTS}
ifneq ($(INFORMATION_MESSAGE), )
	@echo $(INFORMATION_MESSAGE)
endif
	${MAKE}  -f nbproject/Makefile-default.mk dist/${CND_CONF}/${IMAGE_TYPE}/mplab.${IMAGE_TYPE}.${OUTPUT_SUFFIX}

MP_PROCESSOR_OPTION=16f876
MP_LINKER_DEBUG_OPTION=-r=ROM@0x1F00:0x1FFF -r=RAM@SHARE:0x70:0x70 -r=RAM@SHARE:0xF0:0xF0 -r=RAM@SHARE:0x170:0x170 -r=RAM@GPR:0x1E5:0x1EF -r=RAM@SHARE:0x1F0:0x1F0
# ------------------------------------------------------------------------------------
# Rules for buildStep: assemble
ifeq ($(TYPE_IMAGE), DEBUG_RUN)
${OBJECTDIR}/uni-dds-hw9.o: uni-dds-hw9.asm  nbproject/Makefile-${CND_CONF}.mk
	@${MKDIR} "${OBJECTDIR}" 
	@${RM} ${OBJECTDIR}/uni-dds-hw9.o.d 
	@${RM} ${OBJECTDIR}/uni-dds-hw9.o 
	@${FIXDEPS} dummy.d -e "/home/dirk/projects/hw9_digital_vfo/repo/hw9-dds-vfo/hw9/mplab/uni-dds-hw9.ERR" $(SILENT) -c ${MP_AS} $(MP_EXTRA_AS_PRE) -d__DEBUG  -q -p$(MP_PROCESSOR_OPTION) -u  $(ASM_OPTIONS)    \\\"/home/dirk/projects/hw9_digital_vfo/repo/hw9-dds-vfo/hw9/mplab/uni-dds-hw9.asm\\\" 
	@${MV}  /home/dirk/projects/hw9_digital_vfo/repo/hw9-dds-vfo/hw9/mplab/uni-dds-hw9.O ${OBJECTDIR}/uni-dds-hw9.o
	@${MV}  /home/dirk/projects/hw9_digital_vfo/repo/hw9-dds-vfo/hw9/mplab/uni-dds-hw9.ERR ${OBJECTDIR}/uni-dds-hw9.o.err
	@${MV}  /home/dirk/projects/hw9_digital_vfo/repo/hw9-dds-vfo/hw9/mplab/uni-dds-hw9.LST ${OBJECTDIR}/uni-dds-hw9.o.lst
	@${RM}  /home/dirk/projects/hw9_digital_vfo/repo/hw9-dds-vfo/hw9/mplab/uni-dds-hw9.HEX 
	@${DEP_GEN} -d "${OBJECTDIR}/uni-dds-hw9.o"
	@${FIXDEPS} "${OBJECTDIR}/uni-dds-hw9.o.d" $(SILENT) -rsi ${MP_AS_DIR} -c18 
	
else
${OBJECTDIR}/uni-dds-hw9.o: uni-dds-hw9.asm  nbproject/Makefile-${CND_CONF}.mk
	@${MKDIR} "${OBJECTDIR}" 
	@${RM} ${OBJECTDIR}/uni-dds-hw9.o.d 
	@${RM} ${OBJECTDIR}/uni-dds-hw9.o 
	@${FIXDEPS} dummy.d -e "/home/dirk/projects/hw9_digital_vfo/repo/hw9-dds-vfo/hw9/mplab/uni-dds-hw9.ERR" $(SILENT) -c ${MP_AS} $(MP_EXTRA_AS_PRE) -q -p$(MP_PROCESSOR_OPTION) -u  $(ASM_OPTIONS)    \\\"/home/dirk/projects/hw9_digital_vfo/repo/hw9-dds-vfo/hw9/mplab/uni-dds-hw9.asm\\\" 
	@${MV}  /home/dirk/projects/hw9_digital_vfo/repo/hw9-dds-vfo/hw9/mplab/uni-dds-hw9.O ${OBJECTDIR}/uni-dds-hw9.o
	@${MV}  /home/dirk/projects/hw9_digital_vfo/repo/hw9-dds-vfo/hw9/mplab/uni-dds-hw9.ERR ${OBJECTDIR}/uni-dds-hw9.o.err
	@${MV}  /home/dirk/projects/hw9_digital_vfo/repo/hw9-dds-vfo/hw9/mplab/uni-dds-hw9.LST ${OBJECTDIR}/uni-dds-hw9.o.lst
	@${RM}  /home/dirk/projects/hw9_digital_vfo/repo/hw9-dds-vfo/hw9/mplab/uni-dds-hw9.HEX 
	@${DEP_GEN} -d "${OBJECTDIR}/uni-dds-hw9.o"
	@${FIXDEPS} "${OBJECTDIR}/uni-dds-hw9.o.d" $(SILENT) -rsi ${MP_AS_DIR} -c18 
	
endif

# ------------------------------------------------------------------------------------
# Rules for buildStep: link
ifeq ($(TYPE_IMAGE), DEBUG_RUN)
dist/${CND_CONF}/${IMAGE_TYPE}/mplab.${IMAGE_TYPE}.${OUTPUT_SUFFIX}: ${OBJECTFILES}  nbproject/Makefile-${CND_CONF}.mk    
	@${MKDIR} dist/${CND_CONF}/${IMAGE_TYPE} 
	${MP_LD} $(MP_EXTRA_LD_PRE)   -p$(MP_PROCESSOR_OPTION)  -w -x -u_DEBUG -z__ICD2RAM=1 -m"${DISTDIR}/${PROJECTNAME}.${IMAGE_TYPE}.map"   -z__MPLAB_BUILD=1  -z__MPLAB_DEBUG=1 $(MP_LINKER_DEBUG_OPTION) -odist/${CND_CONF}/${IMAGE_TYPE}/mplab.${IMAGE_TYPE}.${OUTPUT_SUFFIX}  ${OBJECTFILES_QUOTED_IF_SPACED}     
else
dist/${CND_CONF}/${IMAGE_TYPE}/mplab.${IMAGE_TYPE}.${OUTPUT_SUFFIX}: ${OBJECTFILES}  nbproject/Makefile-${CND_CONF}.mk   
	@${MKDIR} dist/${CND_CONF}/${IMAGE_TYPE} 
	${MP_LD} $(MP_EXTRA_LD_PRE)   -p$(MP_PROCESSOR_OPTION)  -w  -m"${DISTDIR}/${PROJECTNAME}.${IMAGE_TYPE}.map"   -z__MPLAB_BUILD=1  -odist/${CND_CONF}/${IMAGE_TYPE}/mplab.${IMAGE_TYPE}.${DEBUGGABLE_SUFFIX}  ${OBJECTFILES_QUOTED_IF_SPACED}     
endif


# Subprojects
.build-subprojects:


# Subprojects
.clean-subprojects:

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r build/default
	${RM} -r dist/default

# Enable dependency checking
.dep.inc: .depcheck-impl

DEPFILES=$(shell "${PATH_TO_IDE_BIN}"mplabwildcard ${POSSIBLE_DEPFILES})
ifneq (${DEPFILES},)
include ${DEPFILES}
endif
