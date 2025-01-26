# Configuration
MLB       = lethe.mlb
TARGET    = lethe
SRC_DIR   = lib
FORMATTER = smlfmt

# Check for required tools
MLTON := $(shell command -v mlton 2> /dev/null)
SMLFMT := $(shell command -v $(FORMATTER) 2> /dev/null)

.PHONY: all build test format clean

all: build test

build: check-mlton
	@echo "Building $(TARGET)..."
	@mlton -default-ann 'allowExtendedTextConsts true' $(MLB)
	@echo "Build successful. Binary: ./$(TARGET)"

test: build
	@echo "Running tests..."
	@./$(TARGET) test
	@echo "Tests completed"

format: check-smlfmt
	@echo "Formatting source files..."
	@$(FORMATTER) -allow-extended-text-consts true -max-width 120 --force $(SRC_DIR)/*.sml
	@echo "Formatting complete"

clean:
	@echo "Cleaning up..."
	@rm -f $(TARGET)
	@echo "Clean complete"

check-mlton:
ifndef MLTON
	$(error "mlton is required but not installed. Please install MLton first.")
endif

check-smlfmt:
ifndef SMLFMT
	$(error "smlfmt is required but not installed. Please install smlfmt first.")
endif

