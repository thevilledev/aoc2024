#!/usr/bin/env python3

from typing import Dict, List, Tuple, Set

def execute_logic_gate(gate_type: str, input_left: int, input_right: int) -> int:
    """
    Executes a logical bitwise operation based on the specified gate type.
    """
    if gate_type == "AND":
        result = input_left & input_right
    elif gate_type == "OR": 
        result = input_left | input_right
    elif gate_type == "XOR":
        result = input_left ^ input_right
    else:
        raise ValueError(f"Unsupported gate type: {gate_type}")
    return result

def find_problematic_wires(
    circuit_operations: List[Tuple[str, str, str, str]],
    max_output_wire: str
) -> Set[str]:
    """
    Analyzes circuit operations to identify potentially problematic wire configurations.
    This is all heuristics and very puzzle-specific.
    """
    problematic_wires = set()

    for input_1, gate_type, input_2, output_wire in circuit_operations:
        # Rule 1: Output wires must use XOR gates
        # sum outputs often come from XOR gates, and
        # maybe the highest z could be the final carry bit
        if (output_wire.startswith('z') and 
            gate_type != "XOR" and 
            output_wire != max_output_wire):
            problematic_wires.add(output_wire)

        # Rule 2: XOR between non-special wires
        # design expects XOR mainly for (x?? XOR y??)
        # or (carry XOR partialSum) steps.
        if (gate_type == "XOR" and
            not any(wire.startswith(('x', 'y', 'z')) 
                   for wire in [output_wire, input_1, input_2])):
            problematic_wires.add(output_wire)

        # Rule 3: AND gates must include reference wire
        # idea is that if we see “AND” not related to the first bit,
        # we expect it to feed into an OR for carry logic. If it’s
        # feeding something else, we suspect a miswiring.
        if gate_type == "AND" and "x00" not in [input_1, input_2]:
            for sub_in1, sub_gate, sub_in2, sub_out in circuit_operations:
                if (output_wire in [sub_in1, sub_in2] and 
                    sub_gate != "OR"):
                    problematic_wires.add(output_wire)

        # Rule 4: XOR outputs feeding into OR gates
        # typical full-adder pattern would do (x AND y)
        # OR (carry AND x^y), not (x^y) OR something in certain contexts.
        if gate_type == "XOR":
            for sub_in1, sub_gate, sub_in2, sub_out in circuit_operations:
                if (output_wire in [sub_in1, sub_in2] and 
                    sub_gate == "OR"):
                    problematic_wires.add(output_wire)

    return problematic_wires

def main() -> None:
    """
    Reads circuit specification and identifies problematic wires.
    """
    wire_values: Dict[str, int] = {}
    circuit_operations: List[Tuple[str, str, str, str]] = []
    max_output_wire = "z00"

    # Parse circuit specification file
    with open("inputs/Day24/real.txt", "r", encoding="utf-8") as circuit_file:
        # Read initial wire values
        current_line = circuit_file.readline()
        while current_line != "\n":
            wire_name, value = current_line.split(": ")
            wire_values[wire_name] = int(value)
            current_line = circuit_file.readline()

        # Read gate operations
        for gate_spec in list(circuit_file):
            gate_inputs, output = gate_spec.split("->")
            gate_components = gate_inputs.split()
            output = output.strip()
            circuit_operations.append(tuple(gate_components + [output]))

            # Track highest numbered output wire
            if (output.startswith('z') and 
                int(output[1:]) > int(max_output_wire[1:])):
                max_output_wire = output

    # Analyze circuit and output results
    problematic_wires = find_problematic_wires(circuit_operations, max_output_wire)
    print(",".join(sorted(problematic_wires)))

if __name__ == "__main__":
    main()