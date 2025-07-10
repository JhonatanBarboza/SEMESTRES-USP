CC = riscv64-unknown-elf-as
LD = riscv64-unknown-elf-ld
QEMU = qemu-riscv32

CFLAGS = -march=rv32i -mabi=ilp32
LDFLAGS = -m elf32lriscv

SRC = $(TARGET).s
OBJ = $(TARGET).o
TARGET = aula1

all: $(TARGET)

	$(TARGET): $(OBJ)
		$(LD) $(LDFLAGS) -o $(TARGET) $(OBJ)

$(OBJ): $(SRC)
	$(CC) $(CFLAGS) -o $(OBJ) $(SRC)

run: $(TARGET)
	$(QEMU) $(TARGET)

clean:
	rm -f $(OBJ) $(TARGET)