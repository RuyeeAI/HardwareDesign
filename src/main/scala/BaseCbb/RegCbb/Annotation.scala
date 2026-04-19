package BaseCbb.RegCbb

/**
 * Reg Field description
 * @param byteOffset
 * @param bitOffset
 * @param bitWidth
 * @param name
 * @param resetValue
 * @param accessType
 * @param wrType
 * @param rdAction
 * @param desc
 * @param group
 * @param groupDesc
 * @param volatile
 * @param hasReset
 * @param enumerations
 */
case class RegFieldDescSer(
                            byteOffset: String,
                            bitOffset: Int,
                            bitWidth: Int,
                            name: String,
                            resetValue: BigInt,
                            accessType: String,
                            wrType: String,
                            rdAction: String,
                            desc: String,
                            group: String,
                            groupDesc: String,
                            volatile: Boolean = false,
                            hasReset: Boolean = false,
                            enumerations: Map[BigInt, (String, String)] = Map()
                          )

/**
 * Register is collection of reg fields
 * @param displayName
 * @param deviceName
 * @param baseAddress
 * @param regFields
 */
case class RegistersSer(
                         displayName: String,
                         deviceName: String,
                         baseAddress: BigInt,
                         regFields: Seq[RegFieldDescSer]
                       )
