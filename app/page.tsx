import { Box, Container, Flex, Heading, Section, Text } from "@radix-ui/themes";

export default function Home() {
  return (
    <Container size="2">
      <Flex direction="column" gap="4" className="border border-2 border-red-500">
        <Heading size="9">19tools</Heading>
        <Flex direction="column" gap="4" className="border border-2 border-green-500">
          <Heading size="8">Canadian Immigration Tools</Heading>
          <Flex direction="column" gap="2">
            <Heading size="7">Proof of Relationship</Heading>
            <Text size="2">Put your pictures and their descriptions together. We'll compress the pictures and produce a document that is within the required size requirements.</Text>
          </Flex>
        </Flex>
      </Flex>
    </Container>
  )}