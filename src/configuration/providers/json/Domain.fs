module Infrastructure.Configuration.Providers.Domain.Json

open Microsoft.Extensions.Configuration

type Client = {
    Provider: IConfigurationRoot
}
type Connection = { Paths: string seq }
