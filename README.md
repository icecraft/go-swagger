# go-swagger


## example
```go
//+go-swagger:object
//for sth
type User struct {
    ID int64 `json:"id"`
    // Name is xxx
    // for xxxx
    Name string `json:"name" binding:"required"`  // 注释
    RoleIDs []int `json:"role_ids"`
}

```

## TODO
### P0


### P1
* 采用 property test 进行单元测试


