module ast
    implicit none
    private
    public :: ASTNode, NODE_NUM, NODE_IDENT, NODE_OP, NODE_FUNC
    ! public :: new_num_node, new_ident_node, new_op_node, new_func_node
    ! public :: print_tree ! debug

    integer, parameter :: NODE_NUM   = 0
    integer, parameter :: NODE_IDENT = 1
    integer, parameter :: NODE_OP    = 2
    integer, parameter :: NODE_FUNC  = 3

    type ASTNode
        ! another args...
        integer :: kind = -1 ! NODE_* for nodes
        real(kind=8) :: nval = 0.0d0  ! if NODE_NUM
        character(:), allocatable :: name ! if NODE_IDENT or NODE_FUNC
        character(len=16) :: opkind = "" ! "ADD"
        ! main args
        type(ASTNode), pointer :: left => null()
        type(ASTNode), pointer :: right => null()
    end type ASTNode

end module ast