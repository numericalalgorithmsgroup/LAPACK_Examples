
# Choose what compiler we're going to use
compiler := gfortran
objext := o
F90 := $(compiler)
ifeq ($(compiler),gfortran)
  FFLAGS := -O0 -Wall
endif
ifeq ($(compiler),nagfor)
  FFLAGS := -O0 -ieee=full -dcfuns -C=all -no_underflow_warning
endif
ifeq ($(compiler),ifort)
  ifneq ($(WINDIR),)
    # Looks like we're running on Windows
    FFLAGS := -Od -warn:all
    objext := obj
  else
    FFLAGS := -O0 -warn all
  endif
endif
ifeq ($(compiler),pgf90)
  FFLAGS := -O0 -Minform=warn
endif
ifndef FFLAGS
  $(error Unrecognised compiler: $(compiler))
endif

MKDIR := mkdir -p
RM := rm -f
RMDIR := rm -rf
AR := ar
MV := mv
CP := cp

BUILD_DIR := BUILD_$(compiler)

# Modify the next two lines to locate your LAPACK and BLAS libraries
LIBLAPACK := ../lapack-3.8.0/$(compiler)/liblapack.a
LIBBLAS := ../lapack-3.8.0/$(compiler)/librefblas.a

LIBAUX_SRC_DIR := auxiliary
LIBAUX_BUILD_DIR := $(BUILD_DIR)/auxiliary

LIBAUX := $(LIBAUX_BUILD_DIR)/libaux.a
LIBAUX_SRC := $(wildcard $(LIBAUX_SRC_DIR)/*.f90)
LIBAUX_OBJ := $(notdir $(LIBAUX_SRC:.f90=.$(objext)))
LIBAUX_OBJ := $(addprefix $(LIBAUX_BUILD_DIR)/,$(LIBAUX_OBJ))

MODULE_SRC_DIR := interface_blocks
MODULE_BUILD_DIR := $(BUILD_DIR)/interface_blocks
MODULE_SRC := $(wildcard $(MODULE_SRC_DIR)/*.f90)
MODULE_MODS := $(notdir $(MODULE_SRC:.f90=.mod))
MODULE_MODS := $(addprefix $(MODULE_BUILD_DIR)/,$(MODULE_MODS))
MODULE_MODS := $(filter-out $(MODULE_BUILD_DIR)/lapack_precision.mod,$(MODULE_MODS))
EXAMPLE_DIR := examples
EXAMPLE_DATA_DIR := $(EXAMPLE_DIR)/data
EXAMPLE_BASERES_DIR := $(EXAMPLE_DIR)/baseresults
EXAMPLE_SRC_DIR := $(EXAMPLE_DIR)/source
EXAMPLE_SRC := $(sort $(wildcard ${EXAMPLE_SRC_DIR}/*.f90))
EXAMPLE_OBJ_DIR := $(BUILD_DIR)/OBJECTS
EXAMPLE_EXE_DIR := $(BUILD_DIR)/EXECUTABLES
EXAMPLE_RES_DIR := $(BUILD_DIR)/results
EXAMPLE_DIFFS_DIR := $(BUILD_DIR)/diffs
EXAMPLE_OBJ := $(notdir $(EXAMPLE_SRC))
EXAMPLE_OBJ := $(EXAMPLE_OBJ:.f90=.$(objext))
EXAMPLE_OBJ := $(addprefix $(EXAMPLE_OBJ_DIR)/,$(EXAMPLE_OBJ))
EXAMPLE_EXE := $(notdir $(EXAMPLE_SRC))
EXAMPLE_EXE := $(EXAMPLE_EXE:.f90=.exe)
EXAMPLE_EXE := $(addprefix $(EXAMPLE_EXE_DIR)/,$(EXAMPLE_EXE))
EXAMPLE_RES := $(notdir $(EXAMPLE_SRC))
EXAMPLE_RES := $(EXAMPLE_RES:.f90=.r)
EXAMPLE_RES := $(addprefix $(EXAMPLE_RES_DIR)/,$(EXAMPLE_RES))
EXAMPLE_LINK_LIBS := $(LIBAUX) $(LIBLAPACK) $(LIBBLAS)

all: ndiffs

# Rules to compile interface blocks
$(MODULE_BUILD_DIR)/lapack_precision.mod: $(MODULE_SRC_DIR)/lapack_precision.f90
	@$(MKDIR) $(MODULE_BUILD_DIR)
	(cd $(MODULE_BUILD_DIR); $(F90) $(FFLAGS) -I. -c ../../$<)
$(MODULE_MODS): $(MODULE_BUILD_DIR)/%.mod: $(MODULE_SRC_DIR)/%.f90 $(MODULE_BUILD_DIR)/lapack_precision.mod 
	@$(MKDIR) $(MODULE_BUILD_DIR)
	(cd $(MODULE_BUILD_DIR); $(F90) $(FFLAGS) -I. -c ../../$<)

# A library of auxiliary sorting and matrix printing routines
# used by the LAPACK example programs
$(LIBAUX_OBJ): $(MODULE_MODS)

$(LIBAUX): $(LIBAUX_OBJ)
	$(MKDIR) $(LIBAUX_BUILD_DIR)
	($(AR) rv $@ $^)

$(LIBAUX_OBJ): $(LIBAUX_BUILD_DIR)/%.$(objext): $(LIBAUX_SRC_DIR)/%.f90
	@$(MKDIR) $(LIBAUX_BUILD_DIR)
	($(F90) $(FFLAGS) -I$(MODULE_BUILD_DIR) -c $<; $(MV) $(notdir $@) $@)

# Compile an example program into an object file
$(EXAMPLE_OBJ): $(MODULE_MODS)
$(EXAMPLE_OBJ): $(EXAMPLE_OBJ_DIR)/%.$(objext): $(EXAMPLE_SRC_DIR)/%.f90
	@$(MKDIR) $(EXAMPLE_OBJ_DIR)
	($(F90) $(FFLAGS) -I$(MODULE_BUILD_DIR) -c $<; $(MV) $(notdir $@) $@)
	@$(RM) $(*F)_mod.mod *genmod.mod *genmod.f90

# Link an object file into an executable
$(EXAMPLE_EXE): $(EXAMPLE_EXE_DIR)/%.exe: $(EXAMPLE_OBJ_DIR)/%.$(objext) $(LIBAUX)
	@$(MKDIR) $(EXAMPLE_EXE_DIR)
	($(F90) $(FFLAGS) $< $(EXAMPLE_LINK_LIBS) -o $(notdir $@); $(MV) $(notdir $@) $@)

# Run the executable and send the results to a file. Then compare
# the results with some expected results, and produce a difference
# file (.x) if there are any differences
$(EXAMPLE_RES): $(EXAMPLE_RES_DIR)/%.r: $(EXAMPLE_EXE_DIR)/%.exe
	@$(MKDIR) $(EXAMPLE_RES_DIR)
	@$(MKDIR) $(EXAMPLE_DIFFS_DIR)
	@(bname=`basename -s .r $@`; \
	  datafile=$(EXAMPLE_DATA_DIR)/$${bname}.d; \
	  $(CP) $(EXAMPLE_DATA_DIR)/$${bname}.d $(EXAMPLE_EXE_DIR); \
	  cd $(EXAMPLE_EXE_DIR); \
	  echo "./$(notdir $<) < $${bname}.d > $${bname}.r 2>&1"; \
	  ./$(notdir $<) < $${bname}.d > $${bname}.r 2>&1; \
	  $(RM) $${bname}.d \
	)
	@(bname=`basename -s .r $@`; \
	  $(MV) $(EXAMPLE_EXE_DIR)/$${bname}.r $(EXAMPLE_RES_DIR); \
	  diff -w $(EXAMPLE_BASERES_DIR)/$${bname}.r $@ > $(EXAMPLE_DIFFS_DIR)/$${bname}.x; \
	  if [ -s $(EXAMPLE_DIFFS_DIR)/$${bname}.x ]; then \
	    echo "XXXXXXXXXX $(EXAMPLE_DIFFS_DIR)/$${bname}.x has differences"; \
	  else \
	    $(RM) $(EXAMPLE_DIFFS_DIR)/$${bname}.x; \
	  fi \
	)

# Convenience target so that you can say "make dgetrf_example.r", for example
$(notdir $(EXAMPLE_RES)): %.r: $(EXAMPLE_RES_DIR)/%.r

# Display the number of difference (.x) files produced after running
# all example programs
ndiffs: $(EXAMPLE_RES)
	@(nres=`find $(EXAMPLE_RES_DIR) -name '*.r' | wc -l`; \
	  ndiffs=`find $(EXAMPLE_DIFFS_DIR) -name '*.x' | wc -l`; \
	  nexpected=`find $(EXAMPLE_SRC_DIR) -name '*.f90' | wc -l`; \
	  echo "There are $${nres} out of the expected $${nexpected} result files in directory:"; \
	  echo "  $(EXAMPLE_RES_DIR)"; \
	  echo "There are $${ndiffs} difference files in directory:"; \
	  echo "  $(EXAMPLE_DIFFS_DIR)")

clean:
	$(RMDIR) $(BUILD_DIR)
