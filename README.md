This repository presents a hardware solution to implement a Redactable Signature Scheme (RSS) to sign a file in which some parts could be removed or modified without invalidating the signature of the other parts. This can also be used to verify the integrity of some part (it hasn't been modified) and that is the aim of the present system. Knowing some parts and the signature should not allow to know the other parts. For this, a one-way secure hash function has to be implemented.

It is often done by software solutions but a hardware one is quicker and is not requiring a full generic processor so it's smaller, cheaper and energetically better.

***** Generic parameters *****

The user can specify the word-length and number of leaves with VHDL generic parameters.
The number of leaves has to be a power of two but if one has to implement it for any number they can use the superior power of 2 and complete the set with any fixed values and the verifying step has to know the exact values used in the beggining.

***** Hash function *****

The hash function used in this accelerator is Keccak. It's an open-source solution and various hardware implementations are proposed. One of them is redistribued in this repository so as to make it working properly. Any hash function could be used as long as the correct state-machine is implemented.
Only one instance of the hash function is implemented to make the accelerator smaller.

***** Verification step *****

This step has not been implemanted but it is almost the same architecture as the signature scheme.
