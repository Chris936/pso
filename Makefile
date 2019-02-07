.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

# All modules.
MODS = pso particle particle_update cost_functions

# Default make.
all: compile
	${ERL} pso start -s init stop

# Compile .erl files.
compile: ${MODS:%=%.beam}

# Run program.

# Run tests.
test:
	${ERL} -s pso start 

# Sweep the floors. *~ for emacs backup-files.
clean:
	rm -rf *.beam erl_crash.dump *~
