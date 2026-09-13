# UET

## SCOPE

UET focues primarily on the Backend Scale-out network. Opportunistically considers support for Frontend networks while not preventing applications in the Scale-up network.

## Virtualization

1. Network tunnel techniques that are used today work in the context of UET, because UET uses IP packets.
   - UET packets can be encapsulated within VxLAN and similar tunnels
   - UET packets can carry packets of tunneled protocols
2. Host virtualization can be accomplished using traditional techniques - such as SR-IOV, S-IOV, or others and unique Fabric(IP) Addressed per tenant
3. Deployment focused on the largest scales leverage techniques that simplify the tunnel logic (e.g., structured addressing techniques.)
4. Deployments that need to focus on network packet efficiency may choose not to use encapsulation.

Basic support to support tunneling:

* If UET packets are carried inside a tunnel, congestion information, such as ECN, should be provided to the encapsulated packets.
* The entropy vector of encapsulated UET packets should be copied to the encapsulating packet to ensure the order and load balancing.

## UET Layers

### Semantics sublayer(SES)

Define addressing, authorization, message types, protocols and semantic header formats between endpoints

SES bridges between the user-facing libfabric API and PDS by:

* mapping libfabric API calls to a set of UET communication operations, such as tagged and untagged send / receive, RMA read/write, and atomics.
* It provides optional message ordering, and various optional initiator or target completion notifications.

SES defines 2 protocols for message transmission:

* Rendezvous
  * Used when messages that exceed the available temporary eager buffer limit at the target.
  * The target waits until receiving process has post a matching reception and then trigger a read from the source. **### not understand.**
* Deferrable send 
  * Simply sends message of any size and a target that can not receive it yet sends a message to stop the sender.
  * Later when the buffer is posted, the target sends a resume message to continue the transaction
* 一个是push，一个是pull？

Two fundamental address types:

* relative addressing for P2P 
* Absolute addressing for client-server connections. 

### Packet Delivery Sublayer(PDS)

Transports a stream of packets reliably to the destination FEP and passes them to target’s SES layer for processing.

Generates and interprets ACK and NACK packets to ensure reliable transmission and use ephemeral state to track outstanding packets in the network.

Achieving a scalable reliablility solution that the state retained in the NIC be based on the number of simultaneously active communications - not total number of endpoints in the application. 

* Key Requirement:

  * Extreme scalability

  * Ordered delivery of some packets.

  * Unordered delivery of some packets - particularly of bulk payload - to enable packet spraying

* Packet types:

  * Request

  * Response(ACK, NACK)

  * Control packet

* Large reads is handled as below:

  * Response code for a read request indicates read accepted.

  * Seperate SES response with data is sent using a PDS Request from the target to the initiator

* Packet Delivery Mode:
  * Reliable, unordered delivery (RUD)
  * Reliable, ordered delivery (ROD)
  * Reliable, unordered delivery for idempotent operation (RUDI)
    * take advantage that some date(bulk payload delivery) can be written into memory multiple times up until the final message completion is delivered at the initiator.
    * Can be reordered in the network and replayed due to loass leading them to be delivered more than once to the semantics layer and in any order.
    * no state is required at the receiver
  * Unreliable, unordered delivery(UUD)
* RUDI & UUD are not subject to UET congestion control and special care has to be taken if they sharesame traffic class with either ROD or RUD.

### Congestion Management Sublayer(CMS)

Ensure the packets are transmitted at highest rate while minimizing network congestion. 

Involves combination of mechanisms for window-based congestion control and load balancing 

Fundamental congestion control algorithms:

* Network-Signal based Congestion Control (NSCC) for sender
* Receiver-Controlled Congestion Control (RCCC) for receiver

### Transport Security Sublayer(TSS)

Defines scalable encryption and authentication mechanism for peer to peer as well as client-server communication.

